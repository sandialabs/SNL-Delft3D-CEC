using System;
using System.IO;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using Deltares.UGrid.Entities;
using Deltares.UGrid.Helpers;

namespace Deltares.UGrid.Api
{
    public sealed class UGridApi : IUGridApi
    {
        private int dataSetId;
        private DataSetConventions convention = DataSetConventions.CONV_NULL;
        private double versionNumber = Double.NaN;
        private string branchTypeVariableName = "branch_type";

        private int startIndex = 0;

        private bool fileOpenForReading;
        private bool fileOpenForWriting;

        static UGridApi()
        {
            NativeLibrary.LoadNativeDll(IoNetCfdImports.GRIDDLL_NAME, Path.GetDirectoryName(typeof(UGridApi).Assembly.Location));
        }

        private bool FileOpen
        {
            get { return fileOpenForReading || fileOpenForWriting; }
        }

        public bool IsUGridFile()
        {
            return FileOpen && convention == DataSetConventions.CONV_UGRID;
        }

        public bool CreateFile(string filePath, FileMetaData fileMetaData)
        {
            var mode = (int)NetcdfOpenMode.nf90_write;
            var errorCode = IoNetCfdImports.ionc_create_dll(filePath, ref mode, ref dataSetId);
            if (errorCode != 0) return false;

            var metaData = fileMetaData.CreateMetaData();
            errorCode = IoNetCfdImports.ionc_add_global_attributes_dll(ref dataSetId, ref metaData);
            if (errorCode != 0) return false;

            fileOpenForReading = true;
            fileOpenForWriting = true;

            return true;
        }

        public bool Open(string filePath)
        {
            if (FileOpen)
                Close();

            if (string.IsNullOrEmpty(filePath) || !File.Exists(filePath))
                return false;

            var conventionTypeNumber = 0;

            int mode = (int)NetcdfOpenMode.nf90_nowrite;
            var errorCode = IoNetCfdImports.ionc_open_dll(filePath, ref mode, ref dataSetId, ref conventionTypeNumber, ref versionNumber);
            if (errorCode != 0) return false;

            fileOpenForReading = true;
            fileOpenForWriting = false;

            convention = typeof(DataSetConventions).IsEnumDefined(conventionTypeNumber)
                ? (DataSetConventions) conventionTypeNumber
                : DataSetConventions.CONV_OTHER;

            if (convention == DataSetConventions.CONV_UGRID && versionNumber < 1.0d)
            {
                convention = DataSetConventions.CONV_OTHER;
            }

            return true;
        }

        public bool Close()
        {
            versionNumber = double.NaN;
            convention = DataSetConventions.CONV_NULL;
            fileOpenForReading = false;
            fileOpenForWriting = false;
            return IoNetCfdImports.ionc_close_dll(ref dataSetId) == 0; // check error code
        }

        public double GetVersion()
        {
            return versionNumber;
        }

        public int GetMeshCount()
        {
            int numberOfMeshes = 0;
            IoNetCfdImports.ionc_get_mesh_count_dll(ref dataSetId, ref numberOfMeshes);
            return numberOfMeshes;
        }

        public int GetNumberOfMeshByType(UGridMeshType meshType)
        {
            var numberOfMeshes = 0;
            var type = (int) meshType;
            IoNetCfdImports.ionc_get_number_of_meshes_dll(ref dataSetId, ref type, ref numberOfMeshes);
            return numberOfMeshes;
        }

        public int[] GetMeshIdsByMeshType(UGridMeshType meshType)
        {
            return GetArrayFromIoNetCdf<int>(
                () => GetNumberOfMeshByType(meshType),
                (pointer, count) =>
                {
                    var type = (int) meshType;
                    IoNetCfdImports.ionc_ug_get_mesh_ids_dll(ref dataSetId, ref type, ref pointer, ref count);
                });
        }

        public int GetVarCount(int meshId, GridLocationType locationType)
        {
            var count = 0;
            var type = (int) locationType;
            IoNetCfdImports.ionc_get_var_count_dll(ref dataSetId, ref meshId, ref type, ref count);
            return count;
        }

        public int[] GetVarIds(int meshId, GridLocationType locationType)
        {
            return GetArrayFromIoNetCdf<int>(
                ()=> GetVarCount(meshId, locationType),
                (pointer, count) =>
                {
                    var type = (int)locationType;
                    IoNetCfdImports.ionc_inq_varids_dll(ref dataSetId, ref meshId, ref type, ref pointer, ref count);
                });
        }

        public int GetCoordinateSystemCode()
        {
            int epsgCode = 0;
            IoNetCfdImports.ionc_get_coordinate_system_dll(ref dataSetId, ref epsgCode);
            return epsgCode;
        }

        public int[] GetNetworkIds()
        {
            return GetArrayFromIoNetCdf<int>(
                GetNumberOfNetworks,
                (pointer, count) => IoNetCfdImports.ionc_get_network_ids_dll(ref dataSetId, ref pointer, ref count));
        }

        public int GetNumberOfNetworks()
        {
            var numberOfNetworks = 0;
            IoNetCfdImports.ionc_get_number_of_networks_dll(ref dataSetId, ref numberOfNetworks);
            return numberOfNetworks;
        }

        public DisposableNetworkGeometry GetNetworkGeometry(int networkId)
        {
            var geometryDimensions = new Network1DGeometryDimensions();
            
            IoNetCfdImports.ionc_get_1d_network_nodes_count_dll(ref dataSetId, ref networkId, ref geometryDimensions.NumberOfNodes);
            IoNetCfdImports.ionc_get_1d_network_branches_count_dll(ref dataSetId, ref networkId, ref geometryDimensions.NumberOfBranches);
            IoNetCfdImports.ionc_get_1d_network_branches_geometry_coordinate_count_dll(ref dataSetId, ref networkId, ref geometryDimensions.NumberOfBranchGeometryPoints);

            var numberOfNodes = geometryDimensions.NumberOfNodes;
            var numberOfBranches = geometryDimensions.NumberOfBranches;
            var numberOfGeometryPoints = geometryDimensions.NumberOfBranchGeometryPoints;

            var disposableNetworkGeometry = new DisposableNetworkGeometry();

            // read name
            var type = typeof(DisposableNetworkGeometry);
            var stringBuilder = new StringBuilder(type.GetBufferSize(nameof(DisposableNetworkGeometry.NetworkName)));
            IoNetCfdImports.ionc_get_network_name_dll(ref dataSetId, ref networkId, stringBuilder);
            disposableNetworkGeometry.NetworkName = stringBuilder.ToString();

            // set with empty arrays for setting in io_netcdf
            disposableNetworkGeometry.InitializeWithEmptyData(geometryDimensions);
            
            // get object with pinned object pointers
            var geometry = disposableNetworkGeometry.CreateNetwork1DGeometry();

            // read nodes
            IoNetCfdImports.ionc_read_1d_network_nodes_v1_dll(ref dataSetId, ref networkId, ref geometry.NodeX, ref geometry.NodeY, ref geometry.NodeIds, ref geometry.NodeLongNames, ref geometryDimensions.NumberOfNodes);

            disposableNetworkGeometry.NodesX = geometry.NodeX.CreateValueArray<double>(numberOfNodes);
            disposableNetworkGeometry.NodesY = geometry.NodeY.CreateValueArray<double>(numberOfNodes);

            disposableNetworkGeometry.NodeIds = geometry.NodeIds.CreateValueArray<string>(numberOfNodes, type.GetBufferSize(nameof(DisposableNetworkGeometry.NodeIds)));
            disposableNetworkGeometry.NodeLongNames = geometry.NodeLongNames.CreateValueArray<string>(numberOfNodes, type.GetBufferSize(nameof(DisposableNetworkGeometry.NodeLongNames)));

            // read branches
            IoNetCfdImports.ionc_get_1d_network_branches_v1_dll(ref dataSetId, ref networkId, ref geometry.SourceNodes,
                ref geometry.TargetNodes, ref geometry.BranchLengths, ref geometry.BranchIds,
                ref geometry.BranchLongNames, ref geometry.BranchGeometryCount, ref geometryDimensions.NumberOfBranches,
                ref startIndex);

            disposableNetworkGeometry.NodesFrom = geometry.SourceNodes.CreateValueArray<int>(numberOfBranches);
            disposableNetworkGeometry.NodesTo = geometry.TargetNodes.CreateValueArray<int>(numberOfBranches);
            disposableNetworkGeometry.BranchGeometryNodesCount = geometry.BranchGeometryCount.CreateValueArray<int>(numberOfBranches);
            disposableNetworkGeometry.BranchOrder = geometry.BranchOrder.CreateValueArray<int>(numberOfBranches);
            disposableNetworkGeometry.BranchLengths = geometry.BranchLengths.CreateValueArray<double>(numberOfBranches);
            disposableNetworkGeometry.BranchIds = geometry.BranchIds.CreateValueArray<string>(numberOfBranches, type.GetBufferSize(nameof(DisposableNetworkGeometry.BranchIds)));
            disposableNetworkGeometry.BranchLongNames = geometry.BranchLongNames.CreateValueArray<string>(numberOfBranches, type.GetBufferSize(nameof(DisposableNetworkGeometry.BranchLongNames)));

            // read branch geometry
            IoNetCfdImports.ionc_read_1d_network_branches_geometry_dll(ref dataSetId, ref networkId, ref geometry.BranchGeometryX, ref geometry.BranchGeometryY, ref geometryDimensions.NumberOfBranchGeometryPoints);
            
            disposableNetworkGeometry.BranchGeometryX = geometry.BranchGeometryX.CreateValueArray<double>(numberOfGeometryPoints);
            disposableNetworkGeometry.BranchGeometryY = geometry.BranchGeometryY.CreateValueArray<double>(numberOfGeometryPoints);

            IoNetCfdImports.ionc_get_1d_network_branchorder_dll(ref dataSetId, ref  networkId, ref geometry.BranchOrder, ref  numberOfBranches);
            disposableNetworkGeometry.BranchOrder = geometry.BranchOrder.CreateValueArray<int>(numberOfBranches);

            IoNetCfdImports.ionc_get_1d_network_branchtype_dll(ref dataSetId, ref networkId, ref geometry.BranchTypes, ref numberOfBranches);
            disposableNetworkGeometry.BranchTypes = geometry.BranchTypes.CreateValueArray<int>(numberOfBranches);

            return disposableNetworkGeometry;
        }

        public int WriteNetworkGeometry(DisposableNetworkGeometry networkGeometry)
        {
            var networkId = -1;
            var name = networkGeometry.NetworkName;
            var geometry = networkGeometry.CreateNetwork1DGeometry();
            var geometryDimensions = networkGeometry.CreateNetwork1DGeometryDimensions();

            // create network based on dimensions
            IoNetCfdImports.ionc_create_1d_network_dll(ref dataSetId, ref networkId, name, ref geometryDimensions.NumberOfNodes, ref geometryDimensions.NumberOfBranches, ref geometryDimensions.NumberOfBranchGeometryPoints);

            IoNetCfdImports.ionc_write_1d_network_nodes_v1_dll(ref dataSetId, ref networkId, ref geometry.NodeX, ref geometry.NodeY, ref geometry.NodeIds, ref geometry.NodeLongNames, ref geometryDimensions.NumberOfNodes);

            IoNetCfdImports.ionc_put_1d_network_branches_v1_dll(ref dataSetId, ref networkId, ref geometry.SourceNodes, ref geometry.TargetNodes, ref geometry.BranchIds, ref geometry.BranchLongNames, 
                ref geometry.BranchLengths, ref geometry.BranchGeometryCount, ref geometryDimensions.NumberOfBranches, ref startIndex);

            IoNetCfdImports.ionc_put_1d_network_branchorder_dll(ref dataSetId, ref networkId, ref geometry.BranchOrder, ref geometryDimensions.NumberOfBranches);

            IoNetCfdImports.ionc_put_1d_network_branchtype_dll(ref dataSetId, ref networkId, ref geometry.BranchTypes, ref geometryDimensions.NumberOfBranches);

            IoNetCfdImports.ionc_write_1d_network_branches_geometry_dll(ref dataSetId, ref networkId, ref geometry.BranchGeometryX, ref geometry.BranchGeometryY, ref geometryDimensions.NumberOfBranchGeometryPoints);

            return networkId;
        }

        public int GetNetworkIdFromMeshId(int meshId)
        {
            int networkId = 0;
            IoNetCfdImports.ionc_get_network_id_from_mesh_id_dll(ref dataSetId, ref meshId, ref networkId);
            return networkId;
        }

        public Disposable1DMeshGeometry GetMesh1D(int meshId)
        {
            // get dimensions
            var mesh1dDimensions = new Mesh1DGeometryDimensions();
            IoNetCfdImports.ionc_get_1d_mesh_discretisation_points_count_dll(ref dataSetId, ref meshId, ref mesh1dDimensions.NumberOfNodes);
            IoNetCfdImports.ionc_get_edge_count_dll(ref dataSetId, ref meshId, ref mesh1dDimensions.NumberOfEdges);

            var disposable1DMeshGeometry = new Disposable1DMeshGeometry();
            disposable1DMeshGeometry.InitializeWithEmptyData(mesh1dDimensions);

            // pin in memory
            var mesh1d = disposable1DMeshGeometry.CreateMesh1DGeometry();

            IoNetCfdImports.ionc_get_1d_mesh_discretisation_points_v2_dll(ref dataSetId, ref meshId, ref mesh1d.BranchIds,
                ref mesh1d.BranchOffsets, ref mesh1d.NodeIds, ref mesh1d.NodeLongNames, ref mesh1dDimensions.NumberOfNodes,
                ref startIndex, ref mesh1d.NodeX, ref mesh1d.NodeY);

            // todo : add missing ionc_get_1d_mesh_edges_dll call

            IoNetCfdImports.ionc_get_1d_mesh_edges_dll(ref dataSetId, ref meshId,
                ref mesh1d.EdgeBranchIds, ref mesh1d.EdgeCenterPointOffset,
                ref mesh1dDimensions.NumberOfEdges, ref startIndex, ref mesh1d.EdgeCenterPointX,
                ref mesh1d.EdgeCenterPointY);

            var type = typeof(Disposable1DMeshGeometry);

            var stringBuilder = new StringBuilder(type.GetBufferSize(nameof(disposable1DMeshGeometry.Name)));
            IoNetCfdImports.ionc_get_mesh_name_dll(ref dataSetId, ref meshId, stringBuilder);
            
            disposable1DMeshGeometry.Name = stringBuilder.ToString();
            disposable1DMeshGeometry.NodesX = mesh1d.NodeX.CreateValueArray<double>(mesh1dDimensions.NumberOfNodes);
            disposable1DMeshGeometry.NodesY = mesh1d.NodeY.CreateValueArray<double>(mesh1dDimensions.NumberOfNodes);
            disposable1DMeshGeometry.NodeIds = mesh1d.NodeIds.CreateValueArray<string>(mesh1dDimensions.NumberOfNodes, type.GetBufferSize(nameof(Disposable1DMeshGeometry.NodeIds)));
            disposable1DMeshGeometry.NodeLongNames = mesh1d.NodeLongNames.CreateValueArray<string>(mesh1dDimensions.NumberOfNodes, type.GetBufferSize(nameof(Disposable1DMeshGeometry.NodeLongNames)));
            
            disposable1DMeshGeometry.BranchIDs = mesh1d.BranchIds.CreateValueArray<int>(mesh1dDimensions.NumberOfNodes);
            disposable1DMeshGeometry.BranchOffsets = mesh1d.BranchOffsets.CreateValueArray<double>(mesh1dDimensions.NumberOfNodes);

            disposable1DMeshGeometry.EdgeBranchIds = mesh1d.EdgeBranchIds.CreateValueArray<int>(mesh1dDimensions.NumberOfEdges);
            disposable1DMeshGeometry.EdgeCenterPointOffset = mesh1d.EdgeCenterPointOffset.CreateValueArray<double>(mesh1dDimensions.NumberOfEdges);
            disposable1DMeshGeometry.EdgeCenterPointX = mesh1d.EdgeCenterPointX.CreateValueArray<double>(mesh1dDimensions.NumberOfEdges);
            disposable1DMeshGeometry.EdgeCenterPointY = mesh1d.EdgeCenterPointY.CreateValueArray<double>(mesh1dDimensions.NumberOfEdges);

            return disposable1DMeshGeometry;
        }

        public int WriteMesh1D(Disposable1DMeshGeometry mesh, int networkId)
        {
            int meshId = 0;

            var mesh1d = mesh.CreateMesh1DGeometry();
            var mesh1dDimensions = mesh.CreateMesh1DGeometryDimensions();
            
            var networkName = GetNetworkNameById(networkId);
            var writeXy = 1;
            
            IoNetCfdImports.ionc_create_1d_mesh_v1_dll(ref dataSetId, networkName, ref meshId, mesh.Name,
                ref mesh1dDimensions.NumberOfNodes, ref mesh1dDimensions.NumberOfEdges, ref writeXy);

            IoNetCfdImports.ionc_put_1d_mesh_discretisation_points_v2_dll(ref dataSetId, ref meshId, ref mesh1d.BranchIds,
                ref mesh1d.BranchOffsets, ref mesh1d.NodeIds, ref mesh1d.NodeLongNames, ref mesh1dDimensions.NumberOfNodes,
                ref startIndex, ref mesh1d.NodeX, ref mesh1d.NodeY);

            IoNetCfdImports.ionc_put_1d_mesh_edges_dll(ref dataSetId, ref meshId, ref mesh1d.EdgeBranchIds,
                ref mesh1d.EdgeCenterPointOffset, ref mesh1dDimensions.NumberOfEdges, ref startIndex, ref mesh1d.EdgeCenterPointX, 
                ref mesh1d.EdgeCenterPointY);

            return meshId;
        }

        public Disposable2DMeshGeometry GetMesh2D(int meshId)
        {
            // get dimensions
            var mesh2dDimensions = new Mesh2DGeometryDimensions();

            var networkId = 0;
            IoNetCfdImports.ionc_get_meshgeom_dim_dll(ref dataSetId, ref meshId, ref networkId, ref mesh2dDimensions);
            
            var stringBuilder = new StringBuilder(255);
            IoNetCfdImports.ionc_get_mesh_name_dll(ref dataSetId, ref meshId, stringBuilder);

            var disposable2DMeshGeometry = new Disposable2DMeshGeometry
            {
                Name = stringBuilder.ToString(),
                MaxNumberOfFaceNodes = mesh2dDimensions.maxnumfacenodes
            };

            disposable2DMeshGeometry.InitializeWithEmptyData(mesh2dDimensions);

            var mesh2d = disposable2DMeshGeometry.CreateMeshGeometry();
            
            var includeArrays = true;
            IoNetCfdImports.ionc_get_meshgeom_dll(ref dataSetId, ref meshId, ref networkId, ref mesh2d, ref startIndex, ref includeArrays);

            disposable2DMeshGeometry.NodesX = mesh2d.nodex.CreateValueArray<double>(mesh2dDimensions.numnode);
            disposable2DMeshGeometry.NodesY = mesh2d.nodey.CreateValueArray<double>(mesh2dDimensions.numnode);

            disposable2DMeshGeometry.EdgeNodes = mesh2d.edge_nodes.CreateValueArray<int>(mesh2dDimensions.numedge * 2);
            disposable2DMeshGeometry.FaceNodes = mesh2d.face_nodes.CreateValueArray<int>(mesh2dDimensions.maxnumfacenodes * mesh2dDimensions.numface);
            disposable2DMeshGeometry.FaceX = mesh2d.facex.CreateValueArray<double>(mesh2dDimensions.numface);
            
            return disposable2DMeshGeometry;
        }

        public int WriteMesh2D(Disposable2DMeshGeometry mesh)
        {
            int meshId = 0;
            int networkId = -1;

            var geometry = mesh.CreateMeshGeometry();
            var geometryDimensions = mesh.CreateMeshDimensions();

            var meshName = mesh.Name;
            var networkName = "network";

            IoNetCfdImports.ionc_put_meshgeom_dll(ref dataSetId, ref meshId, ref networkId, ref geometry, ref geometryDimensions, meshName, networkName, ref startIndex);

            return meshId;
        }

        public int GetLinksId()
        {
            var contactsId = 0;
            IoNetCfdImports.ionc_get_contact_id_dll(ref dataSetId, ref contactsId);

            return contactsId;
        }

        public DisposableLinksGeometry GetLinks(int linksId)
        {
            var linkDimensions = new LinksGeometryDimensions();
            
            IoNetCfdImports.ionc_get_contacts_count_dll(ref dataSetId, ref linksId, ref linkDimensions.NumberOfLinks);

            var disposableLinksGeometry = new DisposableLinksGeometry();

            disposableLinksGeometry.InitializeWithEmptyData(linkDimensions);

            var linkGeometry = disposableLinksGeometry.CreateLinksGeometry();

            IoNetCfdImports.ionc_get_mesh_contact_v1_dll(ref dataSetId, ref linksId, ref linkGeometry.Mesh1DFrom,
                ref linkGeometry.Mesh2DTo, ref linkGeometry.LinkType, ref linkGeometry.LinkId,
                ref linkGeometry.LinkLongName, ref linkDimensions.NumberOfLinks, ref startIndex);

            var type = typeof(DisposableLinksGeometry);

            disposableLinksGeometry.Mesh2DTo = linkGeometry.Mesh2DTo.CreateValueArray<int>(linkDimensions.NumberOfLinks);
            disposableLinksGeometry.Mesh1DFrom = linkGeometry.Mesh1DFrom.CreateValueArray<int>(linkDimensions.NumberOfLinks);
            disposableLinksGeometry.LinkType = linkGeometry.LinkType.CreateValueArray<int>(linkDimensions.NumberOfLinks);
            disposableLinksGeometry.LinkId = linkGeometry.LinkId.CreateValueArray<string>(linkDimensions.NumberOfLinks, type.GetBufferSize(nameof(DisposableLinksGeometry.LinkId)));
            disposableLinksGeometry.LinkLongName = linkGeometry.LinkLongName.CreateValueArray<string>(linkDimensions.NumberOfLinks, type.GetBufferSize(nameof(DisposableLinksGeometry.LinkLongName)));

            return disposableLinksGeometry;
        }

        public int WriteLinks(DisposableLinksGeometry links)
        {
            var geometry = links.CreateLinksGeometry();
            var geometryDimensions = links.CreateLinksDimensions();

            var contactId = 0;
            var contactName = "links";
            var firstMesh1dId = GetMeshIdsByMeshType(UGridMeshType.Mesh1D).FirstOrDefault();
            var firstMesh2dId = GetMeshIdsByMeshType(UGridMeshType.Mesh2D).FirstOrDefault();
            var location1D = (int) GridLocationType.UG_LOC_NODE;
            var location2D = (int) GridLocationType.UG_LOC_FACE;

            IoNetCfdImports.ionc_def_mesh_contact_dll(ref dataSetId, ref contactId, contactName,
                ref geometryDimensions.NumberOfLinks, ref firstMesh1dId, ref firstMesh2dId, ref location1D,
                ref location2D);

            IoNetCfdImports.ionc_put_mesh_contact_v1_dll(ref dataSetId, ref contactId, ref geometry.Mesh1DFrom,
                ref geometry.Mesh2DTo, ref geometry.LinkType, ref geometry.LinkId, ref geometry.LinkLongName,
                ref geometryDimensions.NumberOfLinks, ref startIndex);

            return contactId;
        }

        public void Dispose()
        {
            if (FileOpen)
            {
                Close();
            }
        }

        private static T[] GetArrayFromIoNetCdf<T>(Func<int> getSizeFunction, Action<IntPtr, int> setArrayFunction)
        {
            int count = getSizeFunction();

            var handle = GCHandle.Alloc(new T[count], GCHandleType.Pinned);
            var pointer = handle.AddrOfPinnedObject();

            setArrayFunction(pointer, count);

            var result = pointer.CreateValueArray<T>(count);
            handle.Free();

            return result;
        }

        private string GetNetworkNameById(int networkId)
        {
            var bufferSize = typeof(DisposableNetworkGeometry).GetBufferSize(nameof(DisposableNetworkGeometry.NetworkName));
            
            var stringBuilder = new StringBuilder(bufferSize);
            IoNetCfdImports.ionc_get_network_name_dll(ref dataSetId, ref networkId, stringBuilder);
            
            return stringBuilder.ToString();
        }
    }
};