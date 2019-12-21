/** @format */

module.exports = {
  root: true,
  env: {browser: true, es6: true, node: true},
  extends: ['eslint:recommended', 'react-app'],
  parser: 'babel-eslint',
  parserOptions: {
    ecmaFeatures: {experimentalObjectRestSpread: true, jsx: true},
    sourceType: 'module',
  },
  plugins: ['react', 'flowtype'],
  //-------------------------------------------------------------------------
  // > <https://eslint.org/docs/user-guide/configuring>
  // >
  // > "off" or 0 - turn the rule off
  // > "warn" or 1 - turn the rule on as a warning (doesn’t affect exit code)
  // > "error" or 2 - turn the rule on as an error (exit code is 1 when triggered)
  //-------------------------------------------------------------------------
  rules: {
    // https://eslint.org/docs/rules/arrow-parens.html#further-reading
    'arrow-parens': ['error', 'as-needed', {requireForBlockBody: false}],
    'comma-dangle': ['warn', 'only-multiline'],
    curly: ['error', 'multi-line', 'consistent'],
    // eslint and prettier indentation rules might differ for nested blocks
    //indent: ['error', 2],
    'jsx-quotes': ['error', 'prefer-single'],
    'keyword-spacing': ['error', {after: true, before: true}],
    'linebreak-style': ['error', 'unix'],
    //'max-len': ['warn', 80],
    'no-trailing-spaces': ['error'],
    'no-unused-vars': [
      'error',
      {argsIgnorePattern: '^_', varsIgnorePattern: '^_'},
    ],
    'object-curly-spacing': ['error', 'never'],
    quotes: ['error', 'single', {avoidEscape: true}],
    radix: ['error', 'as-needed'],
    semi: ['error', 'always'],
    'space-before-function-paren': [
      'error',
      {
        anonymous: 'never',
        named: 'never',
        asyncArrow: 'always',
      },
    ],
    //-------------------------------------------------------------------------
    // https://github.com/yannickcr/eslint-plugin-react/tree/master/docs/rules
    //-------------------------------------------------------------------------
    'react/jsx-boolean-value': ['error', 'always'],
    'react/jsx-closing-bracket-location': 'error',
    // in ESlint 6.6.0 it's necessary to specify both eventHandlerPrefix
    // and eventHandlerPropPrefix simultaneously
    'react/jsx-handler-names': [
      'warn',
      {eventHandlerPrefix: '_handle', eventHandlerPropPrefix: 'on'},
    ],
    'react/jsx-max-props-per-line': ['warn', {when: 'multiline'}],
    'react/jsx-wrap-multilines': [
      'error',
      {
        declaration: 'parens-new-line',
        assignment: 'parens-new-line',
        return: 'parens-new-line',
        arrow: 'parens-new-line',
        condition: 'parens-new-line',
        logical: 'ignore',
        prop: 'ignore',
      },
    ],
    'react/jsx-uses-react': 'error',
    'react/jsx-uses-vars': 'error',
    'react/no-string-refs': 'error',
    'react/no-unused-prop-types': 'error',
    'react/require-default-props': ['error', {forbidDefaultForRequired: true}],
    'react/self-closing-comp': ['error', {component: true, html: true}],
    'react-native/no-inline-styles': 'off',
    //-------------------------------------------------------------------------
    // https://github.com/gajus/eslint-plugin-flowtype/blob/master/README.md
    //-------------------------------------------------------------------------
    'flowtype/space-after-type-colon': [
      'error',
      'always',
      {allowLineBreak: true},
    ],
    // prettier might split generic type annotation across multiple line
    // and this rule would complain about it
    'flowtype/generic-spacing': ['off'],
  },
};
