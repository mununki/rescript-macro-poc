type ctx = {
  filePath: string,
  moduleName: string,
}

type result = {
  structure: array<Ast.structureItem>,
  signature: array<Ast.signatureItem>,
}
