[@bs.module "console"] external consoledir: ('obj, 'option) => unit = "dir";

let clog = o => consoledir(o, {"depth": "null"});
