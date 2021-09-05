package;

import peapi.ManifestResource;
import mono.ilasm.ExternTypeRef;
import cs.system.collections.ArrayList;
import cs.NativeArray;
import cs.system.reflection.AssemblyName;
import peapi.PrimitiveType;
import peapi.PrimitiveTypeRef;
import mono.ilasm.MethodRef;
import mono.ilasm.LdstrInstr;
import mono.ilasm.SimpInstr;
import mono.ilasm.MethodInstr;
import mono.ilasm.TypeRef;
import mono.ilasm.BaseTypeRef;
import mono.ilasm.MethodDef;
import mono.ilasm.Location;
import mono.ilasm.CodeGen;

class Test {
	public static function main() {
		var t = 'test';
		var cg = new CodeGen("test.exe", false, false, false);
		cg.BeginSourceFile("Test.hx");
		cg.BeginAssemblyRef("mscorlib", new AssemblyName("mscorlib"), peapi.AssemAttr.Retargetable);
		cg.EndAssemblyRef();
        cg.SetThisAssembly("Test", peapi.AssemAttr.Retargetable);
        cg.CurrentCustomAttrTarget = cg.ThisAssembly;
        cg.CurrentDeclSecurityTarget = cg.ThisAssembly;
		cg.BeginTypeDef(peapi.TypeAttr.Public, "Foo", null, null, new Location(11, 0), null);
		var def = new MethodDef(cg, peapi.MethAttr.Static, peapi.CallConv.Default, peapi.ImplAttr.IL, "Bar",
			new mono.ilasm.PrimitiveTypeRef(PrimitiveType.Void, "System.Void"), null, new Location(0, 0), null, cg.CurrentTypeDef);
		cg.CurrentMethodDef.EntryPoint();
		cg.CurrentMethodDef.AddInstr(new LdstrInstr("Hello, World!", new Location(0, 0)));
        var externRef = cg.ExternTable.GetTypeRef("mscorlib", "System.Console", false);
        var strType = new mono.ilasm.PrimitiveTypeRef(PrimitiveType.String, "System.String");
        var voidType = new mono.ilasm.PrimitiveTypeRef(PrimitiveType.Void, "System.Void");
		var args:cs.NativeArray<mono.ilasm.BaseTypeRef.BaseTypeRef> = NativeArray.make(cast strType);
        var methRef  = externRef.GetMethodRef(voidType, peapi.CallConv.Default, "WriteLine", args, 0);
		cg.CurrentMethodDef.AddInstr(new MethodInstr(peapi.MethodOp.call, methRef, new Location(0, 0)));
		cg.CurrentMethodDef.AddInstr(new SimpInstr(peapi.Op.ret, new Location(0, 0)));
		cg.EndMethodDef(new Location(0, 0));
		cg.EndTypeDef();
        
		cg.EndSourceFile();
        cg.AddManifestResource(new ManifestResource("What", NativeArray.make(), ManifestResource.PublicResource));
		cg.Write();
		// trace(cg);
	}
}
