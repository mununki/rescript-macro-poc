# rescript-macro-poc

Small standalone POC project for exercising the native macro implementation in the sibling `../rescript` checkout.

## Assumptions

- The compiler repo lives at `/Users/mununki/github/mununki/rescript`
- That repo has already been built at least once (`make`)

## Commands

```sh
pnpm install
pnpm build
pnpm test
```

The project uses local `link:` dependencies for:

- `ppxlib_res`
- `@rescript/runtime`
- `rescript`

That keeps `node_modules/rescript` pointed at the sibling checkout while letting the POC behave like a small standalone project.
