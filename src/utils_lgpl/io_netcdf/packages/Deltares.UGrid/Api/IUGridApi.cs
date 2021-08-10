using System;

namespace Deltares.UGrid.Api
{
    public interface IUGridApi : IDisposable
    {
        #region Generic

        /// <summary>
        /// Checks if the file is a UGrid file
        /// </summary>
        bool IsUGridFile();

        /// <summary>
        /// Creates a UGrid file and opens it for writing
        /// </summary>
        /// <param name="filePath">File name for NetCDF dataset to be opened.</param>
        /// <param name="fileMetaData">The global metadata of the NetCDF file</param>
        bool CreateFile(string filePath, FileMetaData fileMetaData);

        /// <summary>
        /// Tries to open a NetCDF file and initialize based on its specified conventions.
        /// </summary>
        /// <param name="filePath">File name for netCDF dataset to be opened.</param>
        bool Open(string filePath);

        /// <summary>
        /// Tries to close an open io_netcdf data set.
        /// </summary>
        bool Close();

        /// <summary>
        /// Read the version from the initialized grid nc file 
        /// </summary>
        /// <returns>The version in the initialized grid nc file (or NaN)</returns>
        double GetVersion();

        /// <summary>
        /// Gets the number of mesh from a data set.
        /// </summary>
        /// <returns>Number of meshes.</returns>
        int GetMeshCount();

        /// <summary>
        /// Gets number of meshes of specified <see cref="meshType"/>
        /// </summary>
        /// <param name="meshType">Type of mesh to inquire for</param>
        int GetNumberOfMeshByType(UGridMeshType meshType);

        /// <summary>
        /// Gets mesh ids of specified <see cref="meshType"/>
        /// </summary>
        /// <param name="meshType">Type of mesh to inquire for</param>
        int[] GetMeshIdsByMeshType(UGridMeshType meshType);

        /// <summary>
        /// Gets number of variables depending on specified mesh (<see cref="meshId"/>) and <see cref="locationType"/>
        /// </summary>
        /// <param name="meshId">Id of the mesh</param>
        /// <param name="locationType">Data location type</param>
        int GetVarCount(int meshId, GridLocationType locationType);

        /// <summary>
        /// Gets the names of variables depending on specified mesh (<see cref="meshId"/>) and <see cref="locationType"/>
        /// </summary>
        /// <param name="meshId">Id of the mesh</param>
        /// <param name="locationType">Location type <seealso cref="GridLocationType"/></param>
        /// <returns>Variable indices</returns>
        int[] GetVarIds(int meshId, GridLocationType locationType);

        /// <summary>
        /// Gets the EPSG code (Coordinate system code)
        /// </summary>
        int GetCoordinateSystemCode();

        #endregion

        #region Network geometry

        /// <summary>
        /// Gets the network ids for all networks
        /// </summary>
        int[] GetNetworkIds();

        /// <summary>
        /// Gets the number of declared networks
        /// </summary>
        int GetNumberOfNetworks();
        
        /// <summary>
        /// Retrieves the network geometry for the specified <see cref="networkId"/>
        /// </summary>
        /// <param name="networkId">Id of the network to get</param>
        DisposableNetworkGeometry GetNetworkGeometry(int networkId);

        /// <summary>
        /// Writes a network geometry
        /// </summary>
        /// <param name="geometry">Network geometry to write</param>
        /// <returns>Network id</returns>
        int WriteNetworkGeometry(DisposableNetworkGeometry geometry);

        #endregion

        #region Mesh 1D

        /// <summary>
        /// Gets a network id on which the mesh (<see cref="meshId"/>) depends
        /// </summary>
        /// <param name="meshId">Id of the mesh</param>
        int GetNetworkIdFromMeshId(int meshId);

        /// <summary>
        /// Retrieves the network geometry for the specified <see cref="meshId"/>
        /// </summary>
        /// <param name="meshId">Id of the mesh</param>
        Disposable1DMeshGeometry GetMesh1D(int meshId);

        /// <summary>
        /// Writes a network geometry
        /// </summary>
        /// <param name="mesh">Network geometry to write</param>
        /// <param name="networkId">Network on which the mesh is based</param>
        /// <returns>Mesh id</returns>
        int WriteMesh1D(Disposable1DMeshGeometry mesh, int networkId);

        #endregion

        #region Mesh 2D

        /// <summary>
        /// Reads the 2d mesh for the specified <see cref="meshId"/>
        /// </summary>
        /// <param name="meshId">Id of the mesh to get</param>
        Disposable2DMeshGeometry GetMesh2D(int meshId);

        /// <summary>
        /// Writes the provided 2d mesh
        /// </summary>
        /// <param name="mesh">2d mesh to write</param>
        /// <returns>Mesh id</returns>
        int WriteMesh2D(Disposable2DMeshGeometry mesh);

        #endregion

        #region Links

        /// <summary>
        /// Gets the id of the links
        /// </summary>
        int GetLinksId();

        /// <summary>
        /// Gets the links for the specified <see cref="linksId"/> (see <seealso cref="GetLinksId"/>)
        /// </summary>
        /// <param name="linksId">Id of the links</param>
        DisposableLinksGeometry GetLinks(int linksId);

        /// <summary>
        /// Writes the links and returns the linksId
        /// </summary>
        /// <param name="links">Links to write</param>
        int WriteLinks(DisposableLinksGeometry links);

        #endregion
    }
}