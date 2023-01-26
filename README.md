# nnf

Negation Normal Form manipulation library

### Macro & bit operations

```rust
fn sample() {
    let a = Nnf::Var("a", true);
    let b = var!("b", false);
    let or = or!(a.clone(), b.clone());
}

```

### Simple operations optimization

```rust
assert_eq!(and!("a") & and!("b"), and!("a", "b"));
assert_eq!(and!("a") | and!("b"), or!("a", "b"));
```

### Tseitin Transform (NNF -> CNF)

```rust
fn transform(sentence: Nnf<&'static str>) -> Nnf<&'static str> {
    let mut counter = 0;
    let transformer = TseitinTransform::new(|_| {
        let name: &'static str = Box::leak(Box::new(format!("aux_{}", counter)));
        counter += 1;
        var!(name)
    });

    transformer.transform(sentence)
}

fn test() {
    let sentence = or!(
        and!(
            var!("a"),
            and!("b", "c")
        ),
        and!(
            or!("!d", "e"),
            and!("f", "!g")
        )
    );
    assert!(!sentence.is_cnf(), "Expression is not a cnf yet");

    let sentence = transform(sentence);
    assert!(sentence.is_cnf(), "Expression must be in the cnf form after transformation");

    assert_eq!(
        sentence,
        and!(
        or!("a", "!aux_1"),
        or!("aux_0", "!aux_1"),
        or!("aux_1", "aux_4"),
        or!("aux_2", "!aux_4"),
        or!("aux_3", "!aux_4"),
        or!("!aux_0", "b"),
        or!("!aux_0", "c"),
        or!("aux_3", "d"),
        or!("aux_3", "!e"),
        or!("!aux_2", "f"),
        or!("!aux_2", "!g"),
        or!("!a", "!aux_0", "aux_1"),
        or!("!aux_2", "!aux_3", "aux_4"),
        or!("aux_0", "!b", "!c"),
        or!("!aux_3", "!d", "e"),
        or!("aux_2", "!f", "g")
    )
    );
}
```

## Parse Tree

### Macro support

```rust
fn test() {
    let root = ExpressionNode::Not(
        ExpressionNode::And(
            ExpressionNode::Or(
                ExpressionNode::Leaf(1).into(),
                ExpressionNode::Leaf(2).into(),
            ).into(),
            ExpressionNode::And(
                ExpressionNode::Leaf(3).into(),
                ExpressionNode::Leaf(4).into(),
            ).into(),
        ).into());

    let root = e_not!(
    e_and!(
        e_or!(
            e_leaf!(1),
            e_leaf!(2)
        ),
        e_and!(
            e_leaf!(3),
            e_leaf!(4)
        )
    )
);
}
```

### Expression Tree to NNF conversion (De Morgan's Law, double negation, etc.)

```rust
assert_eq!(
    e_not!(e_leaf!(true) & e_leaf!(true)).to_nnf(),
    e_or!(e_leaf!(false), e_leaf!(false))
);

assert_eq!(
    e_not!(e_leaf!(true) | e_leaf!(true)).to_nnf(),
    e_and!(e_leaf!(false), e_leaf!(false))
);

assert_eq!(
    !ExpressionNode::Not(e_or!(e_leaf!(true), e_leaf!(true)).into()),
    e_or!(e_leaf!(true), e_leaf!(true))
);
```

## (Optional) Render / Graphviz

```rust
fn test() {
    let sentence = or!(
            and!(
                var!("a"),
                and!("b", "c")
            ),
            and!(
                or!("!d", "e"),
                and!("f", "!g")
            )
        );
    assert!(!sentence.render().is_empty());
}
```

