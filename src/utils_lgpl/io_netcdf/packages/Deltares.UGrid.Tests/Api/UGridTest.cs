using System.IO;
using System.Linq;
using Deltares.UGrid.Api;
using NUnit.Framework;

namespace Deltares.UGrid.Tests.Api
{
    [TestFixture]
    public class UGridTest
    {
        private static string Path
        {
            get
            {
                var path = System.IO.Path.GetFullPath(System.IO.Path.Combine(@"..\..\..\..\", "test_data",
                    "river1_full_net.nc"));

                if (!File.Exists(path))
                {
                    Assert.Fail($"Could not find file {path}");
                }

                return path;
            }
        }

        [Test, Ignore]
        [TestCase(UGridMeshType.Mesh1D, 1)]
        [TestCase(UGridMeshType.Mesh2D, 1)]
        [TestCase(UGridMeshType.Combined, 2)]
        [TestCase(UGridMeshType.Mesh3D, 0)]
        public void GivenUGrid_GetNumberOfMeshByType_ShouldWork(UGridMeshType type, int expectedNumber)
        {
            // Arrange & Act
            using (var api = new UGridApi())
            {
                api.Open(Path);
                var numberOfMeshes = api.GetNumberOfMeshByType(type);

                // Assert
                Assert.AreEqual(expectedNumber, numberOfMeshes);
            }
        }

        [Test]
        [TestCase(UGridMeshType.Mesh1D, new[] {1})]
        [TestCase(UGridMeshType.Mesh2D, new[] {2})]
        [TestCase(UGridMeshType.Combined, new[] {1, 2})]
        [TestCase(UGridMeshType.Mesh3D, new int[0])]
        public void GivenUGrid_GetMeshIdsByMeshType_ShouldWork(UGridMeshType type, int[] expectedId)
        {
            // Arrange & Act
            using (var api = new UGridApi())
            {
                api.Open(Path);
                var ids = api.GetMeshIdsByMeshType(type);

                // Assert
                Assert.AreEqual(expectedId, ids);
            }
        }

        [Test]
        public void GivenUGrid_GetMeshCount_ShouldWork()
        {
            // Arrange & Act
            using (var api = new UGridApi())
            {
                api.Open(Path);
                var numberOfMeshes = api.GetMeshCount();

                // Assert
                Assert.AreEqual(2, numberOfMeshes);
            }
        }

        [Test]
        public void GivenUGrid_IsUGridFileAndGetVersion_ShouldWork()
        {
            // Arrange & Act
            using (var api = new UGridApi())
            {
                api.Open(Path);
                var isUGridFile = api.IsUGridFile();
                var version = api.GetVersion();

                // Assert
                Assert.True(isUGridFile, "Should be a UGrid file");
                Assert.AreEqual(1, version);
            }
        }

        [Test]
        [TestCase(1, GridLocationType.UG_LOC_ALL2D, 0)]
        [TestCase(1, GridLocationType.UG_LOC_NODE, 0)]
        [TestCase(1, GridLocationType.UG_LOC_EDGE, 0)]
        [TestCase(2, GridLocationType.UG_LOC_NODE, 2)]
        [TestCase(2, GridLocationType.UG_LOC_EDGE, 3)]
        [TestCase(3, GridLocationType.UG_LOC_ALL2D, 0)]
        public void GivenUGrid_GetVarCount_ShouldWork(int meshId, GridLocationType locationType, int expectedVarCount)
        {
            // Arrange & Act
            using (var api = new UGridApi())
            {
                api.Open(Path);
                var count = api.GetVarCount(meshId, locationType);

                // Assert
                Assert.AreEqual(expectedVarCount, count);
            }
        }

        [Test]
        [TestCase(2, GridLocationType.UG_LOC_NODE, new[] {25, 26})]
        [TestCase(2, GridLocationType.UG_LOC_EDGE, new[] {22, 27, 28})]
        public void GivenUGrid_GetVarIds_ShouldWork(int meshId, GridLocationType locationType, int[] expectedIds)
        {
            // Arrange & Act
            using (var api = new UGridApi())
            {
                api.Open(Path);
                var ids = api.GetVarIds(meshId, locationType);

                // Assert
                Assert.AreEqual(expectedIds, ids);
            }
        }

        [Test]
        public void GivenUGrid_GetCoordinateSystemCode_ShouldWork()
        {
            // Arrange & Act
            using (var api = new UGridApi())
            {
                api.Open(Path);
                var epsgCode = api.GetCoordinateSystemCode();

                // Assert
                Assert.AreEqual(0, epsgCode);
            }
        }

        [Test]
        public void GivenUGrid_GetNetworkIds_ShouldWork()
        {
            // Arrange & Act
            using (var api = new UGridApi())
            {
                api.Open(Path);
                var networkIds = api.GetNetworkIds();

                // Assert
                Assert.AreEqual(new[] {1}, networkIds);
            }
        }

        [Test]
        public void GivenUGrid_GetNumberOfNetworks_ShouldWork()
        {
            // Arrange & Act
            using (var api = new UGridApi())
            {
                api.Open(Path);
                var numberOfNetworks = api.GetNumberOfNetworks();

                // Assert
                Assert.AreEqual(1, numberOfNetworks);
            }
        }

        [Test]
        public void GivenUGrid_GetNetworkIdFromMeshId_ShouldWork()
        {
            // Arrange & Act
            using (var api = new UGridApi())
            {
                api.Open(Path);
                var networkId = api.GetNetworkIdFromMeshId(1);

                // Assert
                Assert.AreEqual(1, networkId);
            }
        }

        [Test]
        public void GivenUGrid_GetNetworkGeometry_ShouldWork()
        {
            // Arrange & Act
            using (var api = new UGridApi())
            {
                api.Open(Path);

                var networkId = 1;

                using (var networkGeometry = api.GetNetworkGeometry(networkId))
                {
                    // Assert
                    Assert.AreEqual("network", networkGeometry.NetworkName);

                    Assert.AreEqual(2, networkGeometry.NodesX.Length);
                    Assert.AreEqual(293.8, networkGeometry.NodesX[0], 0.1);
                    Assert.AreEqual(538.9, networkGeometry.NodesX[1], 0.1);

                    Assert.AreEqual(2, networkGeometry.NodesY.Length);
                    Assert.AreEqual(27.5, networkGeometry.NodesY[0], 0.1);
                    Assert.AreEqual(956.8, networkGeometry.NodesY[1], 0.1);

                    Assert.AreEqual("nodesids", networkGeometry.NodeIds[0]);
                    Assert.AreEqual("nodesids", networkGeometry.NodeIds[1]);

                    Assert.AreEqual("nodeslongNames", networkGeometry.NodeLongNames[0]);
                    Assert.AreEqual("nodeslongNames", networkGeometry.NodeLongNames[1]);

                    Assert.AreEqual(new[] {-1}, networkGeometry.BranchOrder);
                    Assert.AreEqual(new[] {1165.29}, networkGeometry.BranchLengths);
                    Assert.AreEqual(new[] {25}, networkGeometry.BranchGeometryNodesCount);

                    Assert.AreEqual(new[] {1}, networkGeometry.NodesTo);
                    Assert.AreEqual(new[] {0}, networkGeometry.NodesFrom);

                    Assert.AreEqual(new[] {"branchids"}, networkGeometry.BranchIds);
                    Assert.AreEqual(new[] {"branchlongNames"}, networkGeometry.BranchLongNames);

                    var expectedXGeometry = new[]
                    {
                        293.78, 278.97, 265.31, 254.17, 247.44, 248.3, 259.58, 282.24, 314.61, 354.44, 398.94, 445.0,
                        490.6, 532.84, 566.64, 589.08, 600.72, 603.53, 599.27, 590.05, 577.56, 562.97, 547.12, 530.67,
                        538.89
                    };

                    Assert.AreEqual(expectedXGeometry, networkGeometry.BranchGeometryX);

                    var expectedYGeometry = new[]
                    {
                        27.48, 74.87, 122.59, 170.96, 220.12, 269.67, 317.89, 361.93, 399.39, 428.84, 450.76, 469.28,
                        488.89, 514.78, 550.83, 594.93, 643.09, 692.6, 742.02, 790.79, 838.83, 886.28, 933.33, 980.17,
                        956.75
                    };

                    Assert.AreEqual(expectedYGeometry, networkGeometry.BranchGeometryY);
                }
            }
        }

        [Test]
        public void GivenUGrid_WriteNetworkGeometry_ShouldWork()
        {
            var path = System.IO.Path.GetFullPath(System.IO.Path.Combine(".",
                TestContext.CurrentContext.Test.Name + ".nc"));
            if (File.Exists(path))
            {
                File.Delete(path);
            }

            var geometry = new DisposableNetworkGeometry
            {
                NetworkName = "Test_network",

                NodesY = new double[] { 4, 6 },
                NodesX = new double[] { 1, 4 },
                NodeIds = new string[] { "node1", "node2" },
                NodeLongNames = new string[] { "node1 long name", "node2 long name" },

                BranchIds = new string[] { "Branch 1" },
                BranchLongNames = new string[] { "Branch 1 long name" },
                BranchLengths = new double[] { 220.9 },
                BranchOrder = new int[] { 5 },
                BranchTypes = new int[] { 1 },
                NodesFrom = new int[] { 0 },
                NodesTo = new int[] { 1 },
                BranchGeometryNodesCount = new int[] { 4 },

                BranchGeometryX = new double[] { 5, 6, 7, 8 },
                BranchGeometryY = new double[] { 5, 6, 7, 8 }
            };

            // Arrange & Act
            using (var api = new UGridApi())
            {
                api.CreateFile(path, new FileMetaData("Test_model", "Test", "10.4"));

                var networkId = api.WriteNetworkGeometry(geometry);
                
                api.Close();

                Assert.AreEqual(1, networkId);

                api.Open(path);

                var networkIds = api.GetNetworkIds();
                var readGeometry = api.GetNetworkGeometry(networkIds[0]);
                
                Assert.AreEqual(geometry.NetworkName, readGeometry.NetworkName);

                Assert.AreEqual(geometry.NodesY, readGeometry.NodesY);
                Assert.AreEqual(geometry.NodesX, readGeometry.NodesX);
                Assert.AreEqual(geometry.NodeIds, readGeometry.NodeIds);
                Assert.AreEqual(geometry.NodeLongNames, readGeometry.NodeLongNames);

                Assert.AreEqual(geometry.BranchIds, readGeometry.BranchIds);
                Assert.AreEqual(geometry.BranchLongNames, readGeometry.BranchLongNames);
                Assert.AreEqual(geometry.BranchLengths, readGeometry.BranchLengths);
                Assert.AreEqual(geometry.BranchOrder, readGeometry.BranchOrder);
                Assert.AreEqual(geometry.BranchTypes, readGeometry.BranchTypes);
                Assert.AreEqual(geometry.NodesFrom, readGeometry.NodesFrom);
                Assert.AreEqual(geometry.NodesTo, readGeometry.NodesTo);
                Assert.AreEqual(geometry.BranchGeometryNodesCount, readGeometry.BranchGeometryNodesCount);

                Assert.AreEqual(geometry.BranchGeometryX, readGeometry.BranchGeometryX);
                Assert.AreEqual(geometry.BranchGeometryY, readGeometry.BranchGeometryY);
            }
        }

        [Test]
        public void GivenUGrid_GetMesh2D_ShouldWork()
        {
            // Arrange & Act
            using (var api = new UGridApi())
            {
                api.Open(Path);
                var id = api.GetMeshIdsByMeshType(UGridMeshType.Mesh2D).FirstOrDefault();

                var mesh2d = api.GetMesh2D(id);

                Assert.AreEqual("mesh2d", mesh2d.Name);

                Assert.AreEqual(452, mesh2d.NodesX.Length);
                Assert.AreEqual(452, mesh2d.NodesY.Length);

                Assert.AreEqual(1650, mesh2d.EdgeNodes.Length);
                Assert.AreEqual(1500, mesh2d.FaceNodes.Length);

                Assert.AreEqual(375, mesh2d.FaceX.Length);
                Assert.AreEqual(375, mesh2d.FaceY.Length);

                Assert.AreEqual(4, mesh2d.MaxNumberOfFaceNodes);
            }
        }

        [Test]
        public void GivenUGrid_WriteMesh2D_ShouldWork()
        {
            //
            //          7
            //    6.----.----. 8
            //     |    |    | 
            //     |    |    | 
            //    3.----.----. 5
            //     |   4|    | 
            //     |    |    | 
            //     .----.----.
            //     0    1    2
            //
            // 
            var disposable2DMeshGeometry = new Disposable2DMeshGeometry
            {
                Name = "Mesh2d",
                NodesX = new double[] { 1, 2, 3, 1, 2, 3, 1, 2, 3 },
                NodesY = new double[] { 1, 1, 1, 2, 2, 2, 3, 3, 3 },
                EdgeNodes = new []{0,1,1,2,0,3,1,4,2,5,3,4,4,5,3,6,4,7,5,8,6,7,7,8},
                FaceNodes = new []{0,1,3,4,  1,2,4,5,  3,4,6,7  ,4,5,7,8},
                FaceX = new double[] { 1.5, 1.5, 2.5, 2.5 },
                FaceY = new double[] { 1.5, 2.5, 1.5, 2.5 },
                MaxNumberOfFaceNodes = 4
            };

            // Arrange & Act
            using (var api = new UGridApi())
            {
                var path = System.IO.Path.GetFullPath(System.IO.Path.Combine(".", TestContext.CurrentContext.Test.Name + ".nc"));

                api.CreateFile(path, new FileMetaData("Test_model", "Test", "10.4"));

                var meshId = api.WriteMesh2D(disposable2DMeshGeometry);

                Assert.AreEqual(1, meshId);

                api.Close();

                api.Open(path);

                meshId = api.GetMeshIdsByMeshType(UGridMeshType.Mesh2D).First();
                var readMesh2DGeometry = api.GetMesh2D(meshId);

                Assert.AreEqual(disposable2DMeshGeometry.NodesX, readMesh2DGeometry.NodesX);
                Assert.AreEqual(disposable2DMeshGeometry.NodesY, readMesh2DGeometry.NodesY);
                Assert.AreEqual(disposable2DMeshGeometry.EdgeNodes, readMesh2DGeometry.EdgeNodes);
                Assert.AreEqual(disposable2DMeshGeometry.FaceNodes, readMesh2DGeometry.FaceNodes);
                Assert.AreEqual(disposable2DMeshGeometry.FaceX, readMesh2DGeometry.FaceX);
                Assert.AreEqual(disposable2DMeshGeometry.FaceY, readMesh2DGeometry.FaceY);
                Assert.AreEqual(disposable2DMeshGeometry.MaxNumberOfFaceNodes, readMesh2DGeometry.MaxNumberOfFaceNodes);
                Assert.AreEqual(disposable2DMeshGeometry.Name, readMesh2DGeometry.Name);
            }
        }

        [Test]
        public void GivenUGrid_GetMesh1D_ShouldWork()
        {
            // Arrange & Act
            using (var api = new UGridApi())
            {
                api.Open(Path);
                var id = api.GetMeshIdsByMeshType(UGridMeshType.Mesh1D).FirstOrDefault();

                var mesh1D = api.GetMesh1D(id);

                Assert.AreEqual("1dmesh", mesh1D.Name);

                Assert.AreEqual(25, mesh1D.NodesX.Length);
                Assert.AreEqual(25, mesh1D.NodesY.Length);

                Assert.AreEqual(25, mesh1D.NodeIds.Length);
                Assert.AreEqual("meshnodeids", mesh1D.NodeIds[0]);

                Assert.AreEqual(25, mesh1D.NodeLongNames.Length);
                Assert.AreEqual("meshnodelongnames", mesh1D.NodeLongNames[0]);

                var expectedBranchIds = new[] {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
                var expectedBranchOffsets = new[] { 0.0, 49.65, 99.29, 148.92, 198.54, 248.09, 297.62, 347.15, 396.66, 446.19, 495.8, 545.44, 595.08, 644.63, 694.04, 743.52, 793.07, 842.65, 892.26, 941.89, 991.53, 1041.17, 1090.82, 1140.46, 1165.29 };

                Assert.AreEqual(25, mesh1D.BranchIDs.Length);
                Assert.AreEqual(expectedBranchIds, mesh1D.BranchIDs);
                
                Assert.AreEqual(25, mesh1D.BranchOffsets.Length);
                Assert.AreEqual(expectedBranchOffsets, mesh1D.BranchOffsets);
            }
        }
        
        [Test]
        public void GivenUGrid_WriteMesh1D_ShouldWork()
        {
            var path = System.IO.Path.GetFullPath(System.IO.Path.Combine(".",
                TestContext.CurrentContext.Test.Name + ".nc"));

            if (File.Exists(path))
            {
                File.Delete(path);
            }

            var geometry = new DisposableNetworkGeometry
            {
                NetworkName = "Test_network",

                NodesY = new double[] { 4, 6 },
                NodesX = new double[] { 1, 4 },
                NodeIds = new string[] { "node1", "node2" },
                NodeLongNames = new string[] { "node1 long name", "node2 long name" },

                BranchIds = new string[] { "Branch 1" },
                BranchLongNames = new string[] { "Branch 1 long name" },
                BranchLengths = new double[] { 220.9 },
                BranchOrder = new int[] { 5 },
                BranchTypes = new int[] { 1 },
                NodesFrom = new int[] { 0 },
                NodesTo = new int[] { 1 },
                BranchGeometryNodesCount = new int[] { 4 },

                BranchGeometryX = new double[] { 5, 6, 7, 8 },
                BranchGeometryY = new double[] { 5, 6, 7, 8 }
            };

            var disposable1DMeshGeometry = new Disposable1DMeshGeometry
            {
                Name = "Mesh_1d",
                NodesX = new double[] { 1, 2, 3 },
                NodesY = new double[] { 1, 1, 1 },
                BranchIDs = new int[] { 0, 0, 0 },
                BranchOffsets = new double[] { 1, 2, 3 },
                NodeLongNames = new string[] { "Long name 1", "Long name 2", "Long name 3" },
                NodeIds = new string[] { "Node1", "Node2", "Node3" },
                EdgeBranchIds = new int[] {0, 0},
                EdgeCenterPointOffset = new double[] {1.5, 2.5},
                EdgeCenterPointX = new []{1.5,2.5},
                EdgeCenterPointY = new[] { 1.5, 2.5 }
            };

            // Arrange & Act
            using (var api = new UGridApi())
            {
                api.CreateFile(path, new FileMetaData("Test_model", "Test", "10.4"));

                var networkId = api.WriteNetworkGeometry(geometry);
                var meshId = api.WriteMesh1D(disposable1DMeshGeometry, networkId);

                Assert.AreEqual(1, networkId);
                Assert.AreEqual(1, meshId);

                api.Close();
                api.Open(path);

                meshId = api.GetMeshIdsByMeshType(UGridMeshType.Mesh1D).First();
                var readMesh = api.GetMesh1D(meshId);

                Assert.AreEqual(disposable1DMeshGeometry.Name, readMesh.Name);
                Assert.AreEqual(disposable1DMeshGeometry.NodesX, readMesh.NodesX);
                Assert.AreEqual(disposable1DMeshGeometry.NodesY, readMesh.NodesY);
                Assert.AreEqual(disposable1DMeshGeometry.BranchIDs, readMesh.BranchIDs);
                Assert.AreEqual(disposable1DMeshGeometry.BranchOffsets, readMesh.BranchOffsets);
                Assert.AreEqual(disposable1DMeshGeometry.NodeLongNames, readMesh.NodeLongNames);
                Assert.AreEqual(disposable1DMeshGeometry.NodeIds, readMesh.NodeIds);
                Assert.AreEqual(disposable1DMeshGeometry.EdgeBranchIds, readMesh.EdgeBranchIds);
                Assert.AreEqual(disposable1DMeshGeometry.EdgeCenterPointOffset, readMesh.EdgeCenterPointOffset);
                Assert.AreEqual(disposable1DMeshGeometry.EdgeCenterPointX, readMesh.EdgeCenterPointX);
                Assert.AreEqual(disposable1DMeshGeometry.EdgeCenterPointY, readMesh.EdgeCenterPointY);
            }
        }

        [Test]
        public void GivenUGrid_GetLinks_ShouldWork()
        {
            // Arrange & Act
            using (var api = new UGridApi())
            {
                api.Open(Path);
                var linksId = api.GetLinksId();
                var links = api.GetLinks(linksId);

                Assert.AreEqual(23, links.LinkId.Length);
                var expectedFrom = new int[]
                {
                    13, 13, 13, 13, 70, 76, 91, 13, 13, 13, 13, 13, 178, 200, 228, 255, 277, 293, 304, 315, 326, 337,
                    353
                };
                var expectedTo = new int[]
                {
                    1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23
                };

                var expectedContactType = new int[] {3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3};

                Assert.AreEqual(expectedFrom, links.Mesh1DFrom);
                Assert.AreEqual(expectedTo, links.Mesh2DTo);
                Assert.AreEqual(expectedContactType, links.LinkType);
                Assert.AreEqual("linkid", links.LinkId[0]);
                Assert.AreEqual("linklongname", links.LinkLongName[0]);
            }
        }

        [Test]
        public void GivenUGrid_WriteLinks_ShouldWork()
        {
            var path = System.IO.Path.GetFullPath(System.IO.Path.Combine(".",
                TestContext.CurrentContext.Test.Name + ".nc"));

            if (File.Exists(path))
            {
                File.Delete(path);
            }

            // Arrange & Act
            using (var api = new UGridApi())
            {
                api.CreateFile(path, new FileMetaData("Test_model", "Test", "10.4"));

                var disposableLinksGeometry = new DisposableLinksGeometry
                {
                    LinkId = new string[] {"Link id 1"},
                    LinkLongName = new string[] { "Link long name 1" },
                    LinkType = new int[] {3},
                    Mesh2DTo = new int[] {1},
                    Mesh1DFrom = new int[] { 2 }
                };

                var linksId = api.WriteLinks(disposableLinksGeometry);

                Assert.AreEqual(1, linksId);

                api.Close();

                // re-open file
                api.Open(path);

                linksId = api.GetLinksId();
                var readLinksGeometry = api.GetLinks(linksId);

                Assert.AreEqual(disposableLinksGeometry.LinkId, readLinksGeometry.LinkId);
                Assert.AreEqual(disposableLinksGeometry.LinkLongName, readLinksGeometry.LinkLongName);
                Assert.AreEqual(disposableLinksGeometry.LinkType, readLinksGeometry.LinkType);
                Assert.AreEqual(disposableLinksGeometry.Mesh1DFrom, readLinksGeometry.Mesh1DFrom);
                Assert.AreEqual(disposableLinksGeometry.Mesh2DTo, readLinksGeometry.Mesh2DTo);
            }
        }
    }
}