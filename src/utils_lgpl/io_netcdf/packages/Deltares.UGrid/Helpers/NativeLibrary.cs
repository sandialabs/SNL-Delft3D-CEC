using System;
using System.ComponentModel;
using System.IO;
using System.Runtime.InteropServices;
using System.Text;

namespace Deltares.UGrid.Helpers
{
    internal abstract class NativeLibrary : IDisposable
    {
        [DllImport("kernel32", SetLastError = true, CharSet = CharSet.Unicode)]
        public static extern IntPtr LoadLibrary(string lpFileName);

        [DllImport("kernel32", SetLastError = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        public static extern bool FreeLibrary(IntPtr hModule);

        //private: use SwitchDllSearchDirectory with a using instead
        [DllImport("kernel32.dll", CharSet = CharSet.Unicode)]
        public static extern void SetDllDirectory(string lpPathName);

        [DllImport("kernel32.dll", CharSet = CharSet.Unicode, SetLastError = true)]
        private static extern int GetDllDirectory(int nBufferLength, StringBuilder lpPathName);

        public static string GetDllDirectory()
        {
            var tmp = new StringBuilder(4096);
            GetDllDirectory(4096, tmp);
            return tmp.ToString();
        }

        private IntPtr lib = IntPtr.Zero;

        protected NativeLibrary(string fileName)
        {
            lib = LoadLibrary(fileName);
        }

        ~NativeLibrary()
        {
            Dispose();
        }

        public void Dispose()
        {
            if (lib == IntPtr.Zero)
            {
                return;
            }

            FreeLibrary(lib);

            lib = IntPtr.Zero;
        }

        protected IntPtr Library
        {
            get
            {
                if (lib == IntPtr.Zero)
                {
                    throw new InvalidOperationException("NativeLibrary is not loaded");
                }

                return lib;
            }
        }

        /// <summary>
        /// Call this from a static constructor in a class that has DllImport external methods. This 
        /// method uses LoadLibrary to load the correct dll for the current process (32bit or 64bit) 
        /// before DllImport has the chance to resolve the external calls. As long as the dll name is 
        /// the same this works.
        /// </summary>
        /// <param name="dllFileName">The dll file to load.</param>
        /// <param name="baseDirectory">The directory where x64 and x86 are situated.</param>
        public static void LoadNativeDllForCurrentPlatform(string dllFileName, string baseDirectory)
        {
            var platform = Environment.Is64BitProcess ? "x64" : "x86";
            var nativeDirectory = Path.Combine(baseDirectory, platform);

            LoadNativeDll(dllFileName, nativeDirectory);
        }

        /// <summary>
        /// Call this from a static constructor in a class that has DllImport external methods. This 
        /// method uses LoadLibrary to load the correct dll for the current process (32bit or 64bit) 
        /// before DllImport has the chance to resolve the external calls. As long as the dll name is 
        /// the same this works.
        /// </summary>
        /// <param name="dllFileName">The dll file to load.</param>
        /// <param name="directory">The directory to load the dll from.</param>
        public static void LoadNativeDll(string dllFileName, string directory)
        {
            using (SwitchDllSearchDirectory(directory))
            {
                // attempt to load the library
                var ptr = LoadLibrary(dllFileName);
                if (ptr == IntPtr.Zero)
                {
                    var error = Marshal.GetLastWin32Error();
                    var exception = new Win32Exception(error);

                    var messageCouldNotFind = string.Format("{0} {1}.", "Could not find / load", dllFileName);
                    var messageError = string.Format("{0}: {1} - {2}", "Error", error, exception.Message);
                    var messageFile = string.Format("{0}: {1}\\{2}", "File", directory, dllFileName);

                    throw new FileNotFoundException(
                        messageCouldNotFind + Environment.NewLine +
                        messageError + Environment.NewLine +
                        messageFile);
                }
            }
        }

        public static IDisposable SwitchDllSearchDirectory(string dllDirectory)
        {
            return new SwitchDllSearchDirectoryHelper(dllDirectory);
        }

        private class SwitchDllSearchDirectoryHelper : IDisposable // ???
        {
            private readonly string oldDirectory;

            public SwitchDllSearchDirectoryHelper(string dllDirectory)
            {
                oldDirectory = GetDllDirectory();
                SetDllDirectory(dllDirectory);
            }

            public void Dispose()
            {
                SetDllDirectory(oldDirectory);
            }
        }
    }
}