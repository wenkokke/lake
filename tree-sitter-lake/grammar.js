module.exports = grammar({
  name: 'lake',

  rules: {
    source_file: $ => repeat(
      $.declaration,
    ),

    declaration: $ => seq(
      $.identifier,
      '=',
      $.expression,
    ),

    expression: $ => choice(
      $.identifier,
    ),

    identifier: $ => /[a-z]+/
  }
});