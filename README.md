# Geopoly
Erlang lib geopoly wrapper

## How to build

```bash
./rebar compile
```

## To test

```bash
./rebar shell
```

Then

```erl
Erlang/OTP 20 [erts-9.3.1] [source] [64-bit] [smp:4:4] [ds:4:4:10] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V9.3.1  (abort with ^G)
1> geopoly_nif:user_in_region(0.0, 0.0, 4, [[0.0, 0.0], [0.0, 60.0], [60.0, 60.0], [60.0, 0.0]]).
2> geopoly_nif:user_in_region(80.0, 0.0, 4, [[0.0, 0.0], [0.0, 60.0], [60.0, 60.0], [60.0, 0.0]]).
```
