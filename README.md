Purepur
=======

```bash
stack run -- -o test/docs --src "src/**/*.purs" (spago sources)
```

Code-Generation
-----

- `imports` go to the top
- `declarations` go right after the imports (in the same order as in the file)
- `expected outputs` refer to the psci command directly before it