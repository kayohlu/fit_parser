-module(definition_message).

-export([decode/2, get/2]).

decode(DecodedRecordHeader, RecordsBin) ->
    <<ReservedBits:8,
      ArchitectureType:1/integer-unsigned-unit:8,
      GlobalMessageNumberBin:2/binary-unit:8,
      FieldCount:8,
      FieldDefinitions:(3 * FieldCount)/binary-unit:8,
      % DevFieldCount:8,
      % DevFieldDefinition:(3 * DevFieldCount)/binary-unit:8,
      Rest/binary>> =
        RecordsBin,

    GlobalMessageNumber =
        case ArchitectureType of
            0 ->
                <<Num:2/integer-unsigned-little-unit:8>> = GlobalMessageNumberBin,
                Num;
            1 ->
                <<Num:2/integer-unsigned-big-unit:8>> = GlobalMessageNumberBin,
                Num
        end,

    {#{"reserved_bits" => ReservedBits,
       "architecture_type" => ArchitectureType,
       "global_message_number" => GlobalMessageNumber,
       "field_count" => FieldCount,
       "field_definitions" => lists:reverse(decode_field_definitions(FieldDefinitions))},
     % "dev_field_count" => DevFieldCount,
     % "dev_field_definition" => DevFieldDefinition
     Rest}.

decode_field_definitions(FieldDefinitions) ->
    decode_field_definitions(FieldDefinitions, []).

decode_field_definitions(<<>>, Acc) ->
    Acc;
decode_field_definitions(FieldDefinitions, Acc) ->
    <<FieldDefinitionNumber:1/integer-unsigned-unit:8,
      FieldSizeInBytes:1/integer-unsigned-unit:8,
      EndianAbility:1,
      ReservedBits:2,
      BaseTypeNumber:5,
      Rest/binary>> =
        FieldDefinitions,

    FieldDefinition =
        #{"field_definition_number" => FieldDefinitionNumber,
          "field_size_in_bytes" => FieldSizeInBytes,
          "endian_ability" => EndianAbility,
          "base_type_number" => BaseTypeNumber},
    decode_field_definitions(Rest, [FieldDefinition | Acc]).

get(DefinitionMessage, AttributeName) ->
    {_MessageHeader, #{AttributeName := AttributeValue}} = DefinitionMessage,

    AttributeValue.
