type ctx = {
  filePath: string,
  moduleName: string,
}

type context =
  | Pexp
  | Pstr
  | Psig

type result =
  | Pexp(Ast.expression)
  | Pstr(array<Ast.structureItem>)
  | Psig(array<Ast.signatureItem>)
