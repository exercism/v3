Execute the tests with:

```bash
$ yarn test
```

Be sure your code follows best practices, as other users do, with eslint, a tool
to perform static analysis to your code. Tools like this save you some time
detecting typos or silly mistakes in your code:

```bash
$ yarn lint
```

Or do both at the same time:

```bash
$ yarn test && yarn lint
```

The TypeScript track on Exercism does not enforce code _style_, so you're free
to choose between semicolons, tabs vs spaces, and everything inbetween.

## Making Your First Module

Usually, tests on this track will load your implementation importing it as a module: `import Bob from './bob';`. To make it work, you
need to export your implementation from the file the tests are looking for
your module, `bob.ts`:

```typescript
export class Bob {
  public hey(message: string) {
    // Your solution here
  }
}
```

We've provided stubs in each exercise and placed this `export` in there for you.
