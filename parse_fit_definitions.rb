require 'smarter_csv'
require 'erb'

type_definitions = {}

csv = SmarterCSV.process(ARGV[0])

latest_type = nil

csv.each do |row|
  if row.key?(:type_name)
    latest_type = row

    type_definitions[row[:type_name]] = {
      base_type: row[:base_type],
      fields: {}
    }
  else
    fields = type_definitions[latest_type[:type_name]][:fields]
    fields[row[:value]] = row[:value_name]
  end
end

message_definitions = {}

latest_msg = nil
latest_field = nil

csv = SmarterCSV.process(ARGV[1], duplicate_header_suffix: 'dup_head')
# pp csv

csv.each do |row|
  if row.key?(:message_name)
    latest_msg = row

    # p type_definitions['mesg_num'][:fields]
    message_definitions[row[:message_name]] = {
      id: type_definitions['mesg_num'][:fields].invert[row[:message_name]],
      fields: {}
    }
  elsif row.key?(:'field_def_#') # handle non dynamic fields i.e. standard fields that do not depend on the definition of another
    fields = message_definitions[latest_msg[:message_name]][:fields]

    #########################
    # defining what the func_handler should be for the fields in each message
    #
    func_handler = "data_field"


    # A lot of fields in the type definitions are one which return a string value
    # based on some integer.
    # So if the field is a key in the typ definitoins hash then we know it's
    # one of those fields that need to be "looked up", making them an IDField
    # in the RFit world.
    if type_definitions.key?(row[:fit_field_type]) &&
       !%w[date_time local_date_time].include?(row[:fit_field_type]) # ignore the date_times custom types

      func_handler = "id_field"
    end

    # exceptions to the the types in the type definitions that are not just simple look
    # ups i.e. id_fields are:
    func_handler = "message_index_field" if row[:fit_field_type] == 'message_index'
    func_handler = "file_flags_field" if row[:fit_field_type] == 'file_flags'
    func_handler = "date_time_field" if row[:fit_field_type] == 'date_time'
    func_handler = "numeric_field" if (row[:fit_field_type].include?('int') || row[:fit_field_type].include?('float')) &&
                                   row.key?(:units) &&
                                   row[:field_name] != 'fractional_timestamp' &&
                                   !row.key?(:offset)
    func_handler = "byte_field" if row[:fit_field_type] == 'byte' && row.key?(:bits)

    func_handler = "array_field" if row.key?(:array)

    # TODO: Handle numeric data with offsets
    # so far it seems to be only altitude fields that have an offset, and crank_length

    fields[row[:'field_def_#']] ||= {}

    fields[row[:'field_def_#']][row[:field_name]] = row.dup.tap do |hash|
      hash.delete(:example)
      hash.delete(:comment)
      hash.delete(:products)
      hash.delete(:accumulate)
    end.merge(func_handler:)

    latest_field = fields[row[:'field_def_#']][row[:field_name]]
  elsif !row.key?(:'field_def_#') && row.key?(:field_name) # handle sub fields
    # here we handle sub fields, which are fields that are the definitions of the value
    # inside a dynamic field.
    # This means that the latest field is actually a dynamic field. So let's set that as such:
    latest_field[:func_handler] = "dynamic_field"
    row[:ref_field_name] = row[:ref_field_name].split(',').uniq.first
    latest_field[:reference_field] = row[:ref_field_name]


    fields = message_definitions[latest_msg[:message_name]][:fields]



    #########################
    # defining what the func_handler should be for the fields in each message
    #
    func_handler = "data_field"


    # A lot of fields in the type definitions are one which return a string value
    # based on some integer.
    # So if the field is a key in the typ definitoins hash then we know it's
    # own of those fields that need to be "looked up", making them an IDField
    # in the RFit world.
    if type_definitions.key?(row[:fit_field_type]) &&
       !%w[date_time local_date_time].include?(row[:fit_field_type]) # ignore the date_times custom types

      func_handler = "id_field"
    end


    # exceptions to the the types in the type definitions that are not just simple look
    # ups i.e. id_fields are:
    func_handler = "message_index_field" if row[:fit_field_type] == 'message_index'
    func_handler = "file_flags_field" if row[:fit_field_type] == 'file_flags'
    func_handler = "date_time_field" if row[:fit_field_type] == 'date_time'
    func_handler = "numeric_field" if (row[:fit_field_type].include?('int') || row[:fit_field_type].include?('float')) &&
                                   row.key?(:units) &&
                                   row[:field_name] != 'fractional_timestamp' &&
                                   !row.key?(:offset)
    func_handler = "byte_field" if row[:fit_field_type] == 'byte' && row.key?(:bits)

    # TODO: Handle numeric data with offsets
    # so far it seems to be only altitude fields that have an offset, and crank_length

    # fields[latest_field[:'field_def_#']][row[:field_name]] = row.dup.tap do |hash|
    #   hash.delete(:example)
    #   hash.delete(:comment)
    #   hash.delete(:products)
    #   hash.delete(:accumulate)
    #   hash[:ref_field_values] = hash[:ref_field_value].split(",").uniq
    #   hash.delete(:ref_field_value)
    # end.merge(func_handler:)
    latest_field[:sub_fields] ||= []
    latest_field[:sub_fields] << row.dup.tap do |hash|
      hash.delete(:example)
      hash.delete(:comment)
      hash.delete(:products)
      hash.delete(:accumulate)
      hash[:ref_field_values] = hash[:ref_field_value].split(",").uniq
      hash.delete(:ref_field_value)
    end.merge(func_handler:)
      .transform_keys(&:to_s)

  end
end

# pp csv
# pp message_definitions

require "erb"

msg_def_length = message_definitions.length

template = %q{-module(fit_message_definitions).
  -export([message_definition_for/1]).
<% message_definitions.each_with_index do |(message_name, message_definition), idx| %>
<%= "message_definition_for(#{message_definition[:id]}) ->\\n" -%>
<%= '  #{' %>
  <%= "    \\"name\\" => \\"#{message_name}\\"," %>
  <%= "  \\"fields\\" => \\#\\{" %>
  <% field_def_length = message_definition[:fields].length -%>
  <% message_definition[:fields].each_with_index do |(field_num, field_definitions), i| -%>
    <% field_definitions.each do |field_name, field_attrs| -%>
      <% if field_attrs.key?(:sub_fields) -%>
        <%= "    #{field_num} => #\\\{" -%>
        <% field_attrs.each_with_index do |(key, value), fai| -%>
          <% next if key == :"field_def_#" -%>
          <% if key == :sub_fields %>
            <%= "\"#{key}\"" + " =>  [" %>
            <% value_len  = value.length -%>
            <% value.each_with_index.each do |h, z| -%>
                <% if value_len - 1 == z %>
                  <%= "\\##{h}" -%>
                <% else -%>
                  <%= "\\##{h}," -%>
                <% end -%>
            <% end -%>
            <%= ']' %>
          <% else -%>
                <% if field_attrs.length - 1 == fai -%>
                  <%= "    \"#{key.to_s}\" => \"#{value}\"" -%>
                <% else %>
                  <%= "    \"#{key.to_s}\" => \"#{value}\"," -%>
                <% end -%>
          <% end -%>
        <% end -%>
        <% if field_def_length - 1 == i -%>
          <%= '\}' %>
        <% else -%>
          <%= '\},' %>
        <% end -%>
      <% else -%>
        <% field_attrs.delete(:"field_def_#") -%>
        <% if field_def_length - 1 == i -%>
<%= "#{field_num} => \\##{field_attrs.transform_keys(&:to_s).to_s}" -%>
        <% else -%>
<%= "#{field_num} =>  \\##{field_attrs.transform_keys(&:to_s).to_s}," -%>
        <% end %>
      <% end %>
    <% end -%>
  <% end -%>
  <%= '}' %>
  <%= "};" -%>

<% end -%>
  <%= "message_definition_for(_) -> \\#\\{\\}." -%>
}.gsub(/^  /, '')

# template = %q{
# module RFit
#   class Definitions
#     @@definitions = Hash.new { |h, k| h[k] = { name: nil, fields: {} } }

#     def self.definitions
#       @@definitions
#     end

#     def self.add_msg(global_message_number, name)
#       @@definitions[global_message_number][:name] = name
#     end

#     def self.add_field(global_message_number, field_definition_number, name, opts = {})
#       @@definitions[global_message_number][:fields][field_definition_number] = {
#         name:
#       }.merge(opts)
#     end
#   end
# end

# <% message_definitions.each do |message_name, message_definition| %>
# <%= "RFit::Definitions.add_msg(#{message_definition[:id]}, '#{message_name}')" -%>

# <% message_definition[:fields].each do |field_num, field_definitions| -%>
# <% field_definitions.each do |field_name, field_attrs| -%>
# <% field_attrs.delete(:"field_def_#") -%>
# <%= "RFit::Definitions.add_field(#{message_definition[:id]}, #{field_num}, '#{field_name}', #{field_attrs.inspect})" %>
# <% end -%>
# <% end -%>
# <% end -%>
# }.gsub(/^  /, '')


File.write("src/fit_message_definitions.erl", ERB.new(template, trim_mode: '-').result)


type_def_legnth = type_definitions.length

def convert_to_int(num)
  new = ''

  if num[0..1] == '0x'
    new << '16'
    new << '#'
    new << num[2..-1]
  new
  else
    num
  end
end
template = %q{-module(fit_type_definitions).

-compile(export_all).

invalid_value_for("enum") -> 16#FF;
invalid_value_for("sint8") -> 16#7F;
invalid_value_for("uint8") -> 16#FF;
invalid_value_for("sint16") -> 16#7FFF;
invalid_value_for("uint16") -> 16#FFFF;
invalid_value_for("sint32") -> 16#7FFFFFFF;
invalid_value_for("uint32") -> 16#FFFFFFFF;
invalid_value_for("string") -> 16#00;
invalid_value_for("float32") -> 16#FFFFFFFF;
invalid_value_for("float64") -> 16#FFFFFFFFFFFFFFFF;
invalid_value_for("uint8z") -> 16#00;
invalid_value_for("uint16z") -> 16#0000;
invalid_value_for("uint32z") -> 16#00000000;
invalid_value_for("byte") -> 16#FF.

base_type_for("bool") -> "enum";
base_type_for("enum") -> "enum";
base_type_for("sint8") -> "sint8";
base_type_for("uint8") -> "uint8";
base_type_for("sint16") -> "sint16";
base_type_for("uint16") -> "uint16";
base_type_for("sint32") -> "sint32";
base_type_for("uint32") -> "uint32";
base_type_for("string") -> "string";
base_type_for("float32") -> "float32";
base_type_for("float64") -> "float64";
base_type_for("uint8z") -> "uint8z";
base_type_for("uint16z") -> "uint16z";
base_type_for("uint32z") -> "uint32z";
base_type_for("byte") -> "byte";

<% type_definitions.each_with_index do |(type_name, type_definition), idx| %>
  <% if type_def_legnth - 1 == idx %>
  <%= "base_type_for(\"#{type_name}\") -> \\"#{type_definition[:base_type]}\\"." -%>
  <% else -%>
  <%= "base_type_for(\"#{type_name}\") -> \\"#{type_definition[:base_type]}\\";" -%>
  <% end -%>
<% end -%>

% bool is a type that appears in the excel sheet from the SDK
% There is no official base type for it in the excel sheet but a test
% file shows that it has a bae type number of 0, which is `enum`
% So here I will define it:

parse("bool") -> #{ 1 => true, 0 => false };

<% type_definitions.each_with_index do |(type_name, type_definition), idx| %>
<%= "parse(\"#{type_name}\") ->\\n" -%>
<%= '#{' -%>
<% length = type_definition[:fields].length -%>
<% type_definition[:fields].each_with_index do |(fit_value, fit_name), i| %>
  <% if (length - 1) == i -%>
  <%= "  #{convert_to_int(fit_value)} => \"#{fit_name}\"" -%>
  <% else -%>
  <%= "  #{convert_to_int(fit_value)} => \"#{fit_name}\"," -%>
  <% end -%>
<% end %>
  <%= ((type_def_legnth - 1) == idx) ? '}.' : '\};' -%>

<% end -%>
}.gsub(/^  /, '')

File.write("src/fit_type_definitions.erl", ERB.new(template, trim_mode: '-').result)


