-module(data_message).

-export([decode/2, decorate_header/3]).

decode(DefinitionMessage, RecordsBin) ->
    {_,
     #{"field_definitions" := FieldDefinitions,
       "architecture_type" := ArchitectureType,
       "global_message_number" := GlobalMessageNumber}} =
        DefinitionMessage,

    {DataMessageContent, RemainingRecordsBin} =
        decode_contents(RecordsBin, GlobalMessageNumber, FieldDefinitions, ArchitectureType),

    ParsedDataMessageContent =
        maps:fold(fun(FieldDefinitionNumber, RawFieldValue, Acc) ->
                     FieldDefinition =
                         maps:get(FieldDefinitionNumber,
                                  maps:get("fields",
                                           fit_message_definitions:message_definition_for(GlobalMessageNumber),
                                           #{}),
                                  undefined),

                     case FieldDefinition of
                         undefined ->
                             maps:put("raw_field_" ++ integer_to_list(FieldDefinitionNumber),
                                      RawFieldValue,
                                      Acc);
                         _ ->
                             maps:put(
                                 maps:get("field_name", FieldDefinition),
                                 fit_field_handlers:handle_fit_field(GlobalMessageNumber,
                                                                     FieldDefinitionNumber,
                                                                     RawFieldValue),
                                 Acc)
                     end
                  end,
                  maps:new(),
                  DataMessageContent),

    {ParsedDataMessageContent, RemainingRecordsBin}.

decode_contents(RecordsBin, GlobalMessageNumber, FieldDefinitions, ArchitectureType) ->
    decode_contents(RecordsBin, GlobalMessageNumber, FieldDefinitions, ArchitectureType, #{}).

decode_contents(RecordsBin, GlobalMessageNumber, [], ArchitectureType, Acc) ->
    {Acc, RecordsBin};
decode_contents(RecordsBin,
                GlobalMessageNumber,
                FieldDefinitions,
                ArchitectureType,
                Acc) ->
    [#{"base_type_number" := BaseTypeNumber,
       "endian_ability" := EndianAbility,
       "field_definition_number" := FieldDefinitionNumber,
       "field_size_in_bytes" := FieldSizeInBytes}
     | RemainingFieldDefinitions] =
        FieldDefinitions,

    {RawFieldValue, R} =
        fit_field_type_decoder:decode(BaseTypeNumber,
                                      ArchitectureType,
                                      FieldSizeInBytes,
                                      RecordsBin),

    decode_contents(R,
                    GlobalMessageNumber,
                    RemainingFieldDefinitions,
                    ArchitectureType,
                    maps:put(FieldDefinitionNumber, RawFieldValue, Acc)).

decorate_header(AttributeName, AttributeValue, Header) ->
    maps:put(AttributeName, AttributeValue, Header).
