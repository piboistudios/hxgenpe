using System;

namespace Test {
    public class Test {
        public static void Main(string[] args) {
            var x = 0;
            var y = 0.0;
            var inner = new Inner("test");
            inner.Set((x + 5) *2,y);
            System.Console.WriteLine("{0},{1}",inner.x,inner.y);
        }
        class Inner {
            public Inner() {

            }
            public Inner(string s) {
                System.Console.WriteLine(s);
            }
            public void Set(int x, double y) {
                this.x = x;
                this.y = y;
            }
            public int x = 0;
            public double y = 0;
        }
    }
}