
Feature: basic
         Basic conformance tests that all implementations should pass.

# self_eval_zeroish -- Simple self-evaluating forms to zero-ish values.

Scenario: self_eval_int_zero

    When CEL expression "0" is evaluated
    #    int64_value:0
    Then value is IntType(source=0)


Scenario: self_eval_uint_zero

    When CEL expression "0u" is evaluated
    #    uint64_value:0
    Then value is UintType(source=0)


Scenario: self_eval_float_zero

    When CEL expression "0.0" is evaluated
    #    double_value:0
    Then value is DoubleType(source=0)


Scenario: self_eval_float_zerowithexp

    When CEL expression "0e+0" is evaluated
    #    double_value:0
    Then value is DoubleType(source=0)


Scenario: self_eval_string_empty

    When CEL expression "''" is evaluated
    #    string_value:""
    Then value is StringType(source='')


Scenario: self_eval_string_empty_quotes

    When CEL expression '""' is evaluated
    #    string_value:""
    Then value is StringType(source='')


Scenario: self_eval_string_raw_prefix

    When CEL expression 'r""' is evaluated
    #    string_value:""
    Then value is StringType(source='')


Scenario: self_eval_bytes_empty

    When CEL expression 'b""' is evaluated
    #    bytes_value:""
    Then value is BytesType(source=b'')


Scenario: self_eval_bool_false

    When CEL expression "false" is evaluated
    #    bool_value:false
    Then value is BoolType(source=False)


Scenario: self_eval_null

    When CEL expression "null" is evaluated
    #    null_value:NULL_VALUE
    Then value is None


Scenario: self_eval_empty_list

    When CEL expression "[]" is evaluated
    #    list_value:{}
    Then value is []


Scenario: self_eval_empty_map

    When CEL expression "{}" is evaluated
    #    map_value:{}
    Then value is MapType({})


Scenario: self_eval_string_raw_prefix_triple_double

    When CEL expression 'r""""""' is evaluated
    #    string_value:""
    Then value is StringType(source='')


Scenario: self_eval_string_raw_prefix_triple_single

    When CEL expression "r''''''" is evaluated
    #    string_value:""
    Then value is StringType(source='')



# self_eval_nonzeroish -- Simple self-evaluating forms to non-zero-ish values.

Scenario: self_eval_int_nonzero

    When CEL expression "42" is evaluated
    #    int64_value:42
    Then value is IntType(source=42)


Scenario: self_eval_uint_nonzero

    When CEL expression "123456789u" is evaluated
    #    uint64_value:123456789
    Then value is UintType(source=123456789)


Scenario: self_eval_int_negative_min

    When CEL expression "-9223372036854775808" is evaluated
    #    int64_value:-9223372036854775808
    Then value is IntType(source=-9223372036854775808)


Scenario: self_eval_float_negative_exp

    When CEL expression "-2.3e+1" is evaluated
    #    double_value:-23
    Then value is DoubleType(source=-23)


Scenario: self_eval_string_excl

    When CEL expression '"!"' is evaluated
    #    string_value:"!"
    Then value is StringType(source='!')


Scenario: self_eval_string_escape

    When CEL expression "'\''" is evaluated
    #    string_value:"'"
    Then value is StringType(source="'")


Scenario: self_eval_bytes_escape

    When CEL expression "b'ÿ'" is evaluated
    #    bytes_value:"ÿ"
    Then value is BytesType(source=b'\xc3\xbf')


Scenario: self_eval_bytes_invalid_utf8

    When CEL expression "b'\000\xff'" is evaluated
    #    bytes_value:"\x00\xff"
    Then value is BytesType(source=b'\x00\xff')


Scenario: self_eval_list_singleitem

    When CEL expression "[-1]" is evaluated
    #    list_value:{values:{int64_value:-1}}
    Then value is [IntType(source=-1)]


Scenario: self_eval_map_singleitem

    When CEL expression '{"k":"v"}' is evaluated
    #    map_value:{entries:{key:{string_value:"k"} value:{string_value:"v"}}}
    Then value is MapType({StringType(source='k'): StringType(source='v')})


Scenario: self_eval_bool_true

    When CEL expression "true" is evaluated
    #    bool_value:true
    Then value is BoolType(source=True)


Scenario: self_eval_int_hex

    When CEL expression "0x55555555" is evaluated
    #    int64_value:1431655765
    Then value is IntType(source=1431655765)


Scenario: self_eval_int_hex_negative

    When CEL expression "-0x55555555" is evaluated
    #    int64_value:-1431655765
    Then value is IntType(source=-1431655765)


Scenario: self_eval_uint_hex

    When CEL expression "0x55555555u" is evaluated
    #    uint64_value:1431655765
    Then value is UintType(source=1431655765)


Scenario: self_eval_unicode_escape_four

    When CEL expression '"\u270c"' is evaluated
    #    string_value:"✌"
    Then value is StringType(source='✌')


Scenario: self_eval_unicode_escape_eight

    When CEL expression '"\U0001f431"' is evaluated
    #    string_value:"🐱"
    Then value is StringType(source='🐱')


Scenario: self_eval_ascii_escape_seq

    When CEL expression '"\a\b\f\n\r\t\v\"\'\\"' is evaluated
    #    string_value:"\x07\x08\x0c\n\r\t\x0b\"'\\"
    Then value is StringType(source='\x07\x08\x0c\n\r\t\x0b"\'\\')



# variables -- Variable lookups.

Scenario: self_eval_bound_lookup

   #     type:{primitive:INT64}
   Given type_env parameter "x" is TypeType(value='INT64')

   #     int64_value:123
   Given bindings parameter "x" is IntType(source=123)

    When CEL expression "x" is evaluated
    #    int64_value:123
    Then value is IntType(source=123)


Scenario: self_eval_unbound_lookup
          An unbound variable should be marked as an error during execution. See google/cel-go#154
    When CEL expression "x" is evaluated
    #    errors:{message:"undeclared reference to 'x' (in container '')"}
    Then eval_error is "undeclared reference to 'x' (in container '')"


Scenario: unbound_is_runtime_error
          Make sure we can short-circuit around an unbound variable.
    When CEL expression "x || true" is evaluated
    #    bool_value:true
    Then value is BoolType(source=True)



# functions -- Basic mechanisms for function calls.

Scenario: binop

    When CEL expression "1 + 1" is evaluated
    #    int64_value:2
    Then value is IntType(source=2)


Scenario: unbound

    When CEL expression "f_unknown(17)" is evaluated
    #    errors:{message:"unbound function"}
    Then eval_error is 'unbound function'


Scenario: unbound_is_runtime_error

    When CEL expression "f_unknown(17) || true" is evaluated
    #    bool_value:true
    Then value is BoolType(source=True)



# reserved_const -- Named constants should never be shadowed by identifiers.

Scenario: false

   #     type:{primitive:BOOL}
   Given type_env parameter "false" is TypeType(value='BOOL')

   #     bool_value:true
   Given bindings parameter "false" is BoolType(source=True)

    When CEL expression "false" is evaluated
    #    bool_value:false
    Then value is BoolType(source=False)


Scenario: true

   #     type:{primitive:BOOL}
   Given type_env parameter "true" is TypeType(value='BOOL')

   #     bool_value:false
   Given bindings parameter "true" is BoolType(source=False)

    When CEL expression "true" is evaluated
    #    bool_value:true
    Then value is BoolType(source=True)


Scenario: null

   #     type:{primitive:BOOL}
   Given type_env parameter "null" is TypeType(value='BOOL')

   #     bool_value:true
   Given bindings parameter "null" is BoolType(source=True)

    When CEL expression "null" is evaluated
    #    null_value:NULL_VALUE
    Then value is None