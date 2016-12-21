

## Operators

Most of the operators used throughout the suite are taken from standard libraries. Here is an overview.


### Adding values

- `<>` Join (*semigroup* or *monoid*) values together


### Function composition

- `.` Compose functions or lenses
`$`
`&`
`<*>`
`<$>`
`<&>`

### Get, set, view

- `^.` Get a value
- `.~` Set a value
- `%~` Update a value

- TODO traversals, index

- `+~` `-~` `*~` `/~` `<>~` Apply an operation to a value
- `+=` `-=` `*=` `/=` `<>=` Apply an operation to a value

### Points and vectors

- `^+^` `^-^` Vector addition
- `^*`  `^/`  Vector scaling
- `.+^` Point translation
- `.-.` Point difference
