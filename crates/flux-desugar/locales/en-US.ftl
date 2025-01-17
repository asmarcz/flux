# Desugar

desugar_unresolved_var =
    cannot find value `{$var}` in this scope
    .label = not found in this scope

desugar_duplicate_param =
    the name `{$name}` is already used as a parameter
    .label = already used
    .first_use = first use of `{$name}`

desugar_unresolved_sort =
    cannot find sort `{$sort}` in this scope
    .label = not found in this scope

desugar_int_too_large =
    integer literal is too large

desugar_unexpected_literal =
    unexpected literal

desugar_invalid_dot_var =
    unsupported field access in refinement

desugar_sort_arity_mismatch =
    this sort constructor takes {$expected ->
        [0] 0 sort arguments
        [one] 1 sort argument
        *[other] 1 or {$expected} sort arguments
    } but {$found ->
        [one] {$found} was found
        *[other] {$found} were found
    }
    .label = expected {$expected ->
        [0] 0 sort arguments
        [one] {$expected} argument
        *[other] 1 or {$expected} arguments
    }, found {$found}

desugar_refine_arg_count_mismatch =
    this type takes {$expected ->
        [0] 0 refinement arguments
        [one] 1 refinement argument
        *[other] 1 or {$expected} refinement arguments
    } but {$found ->
        [one] {$found} was found
        *[other] {$found} were found
    }
    .label = expected {$expected ->
        [0] 0 refinement arguments
        [one] {$expected} argument
        *[other] 1 or {$expected} arguments
    }, found {$found}

desugar_invalid_unrefined_param =
    invalid use of refinement parameter
    .label = parameter `{$var}` refers to a type with no indices

desugar_illegal_binder =
    illegal binder
    .label = `{$kind}` binder not allowed in this position

desugar_invalid_numeric_suffix =
    invalid suffix `{$suffix}` for number literal
    .label = the suffix must be one of the numeric sorts `int` or `real`

desugar_refined_unrefinable_type =
    type cannot be refined

# Resolve errors

desugar_unsupported_signature =
    unsupported function signature
    .note = {$note}

desugar_unresolved_path =
    cannot resolve `{$path}`
    .help = flux can only resolve a path if it is present in the definition being refined

desugar_unresolved_generic_param =
    cannot resolve generic param
    .note = generic parameters in refined signature must much rust signature
