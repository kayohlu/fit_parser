-module(fit_field_handlers).

-compile(export_all).

handle_fit_field(GlobalMessageNumber, FieldDefinitionNumber, RawValue) ->
    FieldDefinition =
        maps:get(FieldDefinitionNumber,
                 maps:get("fields",
                          fit_message_definitions:message_definition_for(GlobalMessageNumber),
                          #{}),
                 undefined),

    case FieldDefinition of
        undefined ->
            RawValue;
        FieldDef ->
            FitBaseType =
                fit_type_definitions:base_type_for(
                    maps:get("fit_field_type", FieldDef)),
            case RawValue =:= fit_type_definitions:invalid_value_for(FitBaseType) of
                true ->
                    invalid;
                false ->
                    FuncHandler = maps:get("func_handler", FieldDef),
                    apply(?MODULE, list_to_atom(FuncHandler), [FieldDef, RawValue])
            end
    end.

data_field(FieldDefinition, RawValue) ->
    RawValue.

date_time_field(FieldDefinition, RawValue) ->
    calendar:system_time_to_universal_time(calendar:rfc3339_to_system_time("1989-12-31T00:00:00+00:00",
                                                                           [{unit, second}])
                                           + RawValue,
                                           second).

numeric_field(FieldDefinition, RawValue) ->
    RawValue / maps:get("scale", FieldDefinition, 1).

message_index_field(FieldDefinition, RawValue) ->
    RawValue.

array_field(FieldDefinition, RawValue) ->
    RawValue.

dynamic_field(FieldDefinition, RawValue) ->
    RawValue.

file_flags_field(FieldDefinition, RawValue) ->
    RawValue.

id_field(FieldDefinition, RawValue) ->
    maps:get(RawValue,
             fit_type_definitions:parse(
                 maps:get("fit_field_type", FieldDefinition)),
             undefined).
