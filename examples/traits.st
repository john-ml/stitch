-- Typing rule for addition
E1 + E2 : T <== E1 : T, E2 : T, T implements add.

-- Typing rule for coercions
E as T1 : T1.

-- Some numeric literals
1 : T <== T implements add.
2 : T <== T implements add.
3 : T <== T implements add.

-- Some trait implementations
i32 implements add.
i16 implements add.

-- Infer some types
1 + (x as i32) : T? -- OK: T = i32
1 + (x as i8) : T? -- Not OK: i8 does not implement add
