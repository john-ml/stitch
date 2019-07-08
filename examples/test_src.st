quadruple(x i32) i32 =
  y as i32 = x + x;
  z as i32 = x * 2;
  y + z

octuple(z i32) i32 =
  z as i32 = quadruple(x);
  z + z

deref(p *i32) i32 = *p

index(p **i32, i i32) i32 = p[i][i]

dup(x i32) {l i32, r i32} =
  {l = x, r = x}

proj[R](p {x i32; R}) i32 = p.x

id[A](x A) A = x

id_i32(x i32) i32 = id@[i32](x)

inl[C](x i32) <l i32; C> =
  l@x

inr[C](x i32) <r i32; C> =
  r@x

nested(x i32) i32 =
  y as i32 = (
    z as i32 = x + x;
    z := z + 2;
    z
  );
  y + y

main(argc i32, argv **i8) void =
  pri32("hello, world\n")

extract(xy <l i32, r i32>) i32 =
  res as i32 =
    case xy {
      l x -> x,
      r y -> y
    };
  res

quadruple(x) =
  y = x + x;
  z = x * 2;
  y + z

octuple(z) =
  z = quadruple(x);
  z + z

deref(p) = *p

index(p, i) = p[i][i]

dup(x) =
  {l = x, r = x}

proj(p) = p.x

inl(x) =
  l@x

inr(x) =
  r@x

nested(x) =
  y = (
    z = x + x;
    z := z + 2;
    z
  );
  y + y

extract(xy) =
  res =
    case xy {
      l x -> x,
      r y -> y
    };
  res

extract(xy) =
  case xy {
    l x -> x,
    r y -> y
  }

andb(p, q) = if p then q else false

# type vec(A) = {len u64, cap u64, data *A}
