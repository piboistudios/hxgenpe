package haxe.compiler.backends.pe;


class Tools {
    public static function toClrPath(path:Array<String>) {
        if(path[0] == 'cs') path.shift();
        return path.map(part -> part.substr(0, 1).toUpperCase() + part.substr(1)).join('.');
    }
}
