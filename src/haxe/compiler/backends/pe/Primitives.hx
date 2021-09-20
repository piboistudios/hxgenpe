package haxe.compiler.backends.pe;

import peapi.PrimitiveType;
import mono.ilasm.PrimitiveTypeRef;
import mono.ilasm.BaseTypeRef;

enum abstract Primitives(BaseTypeRef) {
    public static var INT = new PrimitiveTypeRef(PrimitiveType.Int32, "System.Int32");
    public static var BOOL = new PrimitiveTypeRef(PrimitiveType.Boolean, "System.Boolean");
    public static var FLOAT = new PrimitiveTypeRef(PrimitiveType.Float64, "System.Double");
    public static var CHAR = new PrimitiveTypeRef(PrimitiveType.Char, "System.Char");
    public static var VOID = new PrimitiveTypeRef(PrimitiveType.Void, "System.Void");
    public static var BYTE = new PrimitiveTypeRef(PrimitiveType.UInt8, "System.Byte");


}