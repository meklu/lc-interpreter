# LC Interpreter

This is a lambda calculus interpreter written in Haskell.
TBD: Everything

The lambda symbol shall be represented in code as the backslash character, `\`,
e.g. `\x.xz`.

Whitespace (`"\t\n\r "`) is ignored.

# Notes

Since this is not quite finished, here are a couple of usage notes in case
anything here is of interest:

To get to an interactive prompt, run the following under `src/`:

    stack ghci Lex.hs

To generate a token stream from an LC program in the interactive Haskell
prompt, you may run something like this:

    tokenizeExpr "(\\x.xyz)"

To generate a tree you may run this:

    generateTree $ tokenizeExpr "foo"

If you want prettier output along with some hopefully useful debug info,
run the following:

    putStr $ dbgTree "\\x.xyz"

# TODO

NB: In these notes `<…>` are arbitrary lambda expressions.

- [ ] LC parser
	- [x] Tokenization
		- Should be mostly done. If extended, needs work :)
	- [ ] Syntax tree generation
		- [ ] Basic statements/operations
			- [x] Variable (essentially passthrough)
				- `x`
			- [x] Abstraction
				- `(\x.<Phi>)`
				- `\xy.<Phi>` → `\x.\y.<Phi>`
			- [x] Application
				- `(<Phi><Psi>)`
				- Needs to be made left-associative
		- [ ] Correct sub-expression grouping
		- [ ] Substitution
			- `[x:=z]\y.x` → `\y.z`
			- `[x:=\z.z]\y.x` → `\y.\z.z`
- [ ] LC interpreter
	- [ ] ß-reduction of statements

		Thought: If `<Phi>` reduces to something alpha congruent to
		`<Phi>`, we could probably detect that during compilation to
		avoid infinite recursion. Probably.

- [ ] Fancy extensions
	- [ ] Arithmetic (parser || evaluator)

		We *could* extend our syntax tree with these types of nodes and
		associate them with math operations

	- [ ] Longer named variables with backtick notation

		e.g. ``\x.(x`foo bar zot`)``

		- [x] Tokenizer
