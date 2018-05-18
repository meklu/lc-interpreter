# LC Interpreter

This is a lambda calculus interpreter written in Haskell.
TBD: Everything

The lambda symbol shall be represented in code as the backslash character, `\`,
e.g. `\x.xz`.

Whitespace (`"\t\n\r "`) is ignored.

# TODO

NB: In these notes `<…>` are arbitrary lambda expressions.

- [ ] LC parser
	- [x] Tokenization
		- Should be mostly done. If extended, needs work :)
	- [ ] Syntax tree generation
		- [ ] Basic statements/operations
			- [ ] Variable (essentially passthrough)
				- `x`
			- [ ] Abstraction
				- `(\x.<Phi>)`
				- `\xy.<Phi>` → `\x.\y.<Phi>`
			- [ ] Application
				- `(<Phi><Psi>)`
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
