# rescript-macro-poc

Small standalone POC project for exercising the native macro implementation in the sibling `../rescript` checkout.

## Assumptions

- The compiler repo lives at `/Users/mununki/github/mununki/rescript`
- That repo has already been built at least once (`make`)

## Local Setup

If you want to test this locally against your own checkout:

1. Clone `https://github.com/mununki/rescript`
2. Build that checkout to branch `poc-macro` at least once with `make`
3. Update the `link:` paths in [package.json](/Users/mununki/github/mununki/rescript-macro-poc/package.json) so they point at your local `rescript` checkout
4. Run `pnpm install`

For example, these dependencies may need to be adjusted:

- `rescript`
- `@rescript/runtime`
- `ppxlib_res`

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
