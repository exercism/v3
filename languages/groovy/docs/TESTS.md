## Running Tests

Execute the test specification with:

```bash
$ gradle test
```

## Enabling More Tests

Once your code passes all active tests, enable another test by removing the
`@Ignore` preceding the next test that interests you.  Do this by deleting
the `@Ignore` line or comment it out by adding two slashes to the start of
the line.

```Groovy
//@Ignore
def 'outputs "Hello, Alice!" when given the name "Alice"'() {
    expect: hello.hello('Alice') == 'Hello, Alice!'
}
```
