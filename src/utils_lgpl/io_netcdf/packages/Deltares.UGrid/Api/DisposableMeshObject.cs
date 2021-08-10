using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.InteropServices;
using Deltares.UGrid.Helpers;

namespace Deltares.UGrid.Api
{
    public abstract class DisposableMeshObject : IDisposable
    {
        private readonly Dictionary<object,GCHandle> objectGarbageCollectHandles = new Dictionary<object, GCHandle>();

        protected bool IsMemoryPinned
        {
            get { return objectGarbageCollectHandles.Count > 0; }
        }
        
        /// <inheritdoc/>
        public void Dispose()
        {
            UnPinMemory();
        }

        protected IntPtr GetPinnedObjectPointer(object objectToLookUp)
        {
            if (!IsMemoryPinned)
            {
                PinMemory();
            }

            return objectGarbageCollectHandles[objectToLookUp].AddrOfPinnedObject();
        }

        protected void PinMemory()
        {
            var arrayFields = GetType().GetFields().Where(f => f.FieldType.IsArray);
            
            // force initialization
            foreach (var arrayField in arrayFields)
            {
                var elementType = arrayField.FieldType.GetElementType();
                var objectToPin = arrayField.GetValue(this);

                if (objectToPin == null)
                {
                    objectToPin = Array.CreateInstance(elementType, 0);
                    arrayField.SetValue(this, objectToPin);
                }

                if (elementType == typeof(string))
                {
                    var bufferSize = GetType().GetBufferSize(arrayField.Name);
                    if (bufferSize == 0) continue;

                    var bytes = ((string[])objectToPin).GetFlattenedAsciiCodedStringArray(bufferSize);
                    AddObjectToPin(bytes, objectToPin);
                }
                else
                {
                    AddObjectToPin(objectToPin);
                }
            }
        }

        private void UnPinMemory()
        {
            foreach (var valuePair in objectGarbageCollectHandles)
            {
                valuePair.Value.Free();
            }

            objectGarbageCollectHandles.Clear();
        }

        private void AddObjectToPin(object objectToPin, object lookupObject = null)
        {
            var key = lookupObject ?? objectToPin;
            objectGarbageCollectHandles.Add(key, GCHandle.Alloc(objectToPin, GCHandleType.Pinned));
        }
    }
}