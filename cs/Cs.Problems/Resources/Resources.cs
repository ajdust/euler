using System.IO;
using System.Reflection;

namespace Cs.Problems.Resources
{
    public static class Resources
    {
        public static string Get(string name)
        {
            var assembly = Assembly.GetExecutingAssembly();
            var resourceName = "Cs.Problems.Resources." + name;

            using (var stream = assembly.GetManifestResourceStream(resourceName))
            using (var reader = new StreamReader(stream))
            {
                return reader.ReadToEnd();
            }
        }
    }
}
