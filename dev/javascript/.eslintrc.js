/** @format */

module.exports = {
  env: {browser: true, es6: true, node: true},
  extends: ['eslint:recommended', 'plugin:flowtype/recommended'],
  parser: 'babel-eslint',
  parserOptions: {
    ecmaFeatures: {experimentalObjectRestSpread: true, jsx: true},
    sourceType: 'module',
  },
  plugins: ['react', 'flowtype'],
  rules: {
    //-------------------------------------------------------------------------
    // https://eslint.org/docs/rules/
    //-------------------------------------------------------------------------
    // https://eslint.org/docs/rules/arrow-parens.html#further-reading
    'arrow-parens': ['error', 'as-needed', {requireForBlockBody: false}],
    'comma-dangle': ['warn', 'only-multiline'],
    indent: ['error', 2],
    'keyword-spacing': ['error', {after: true, before: true}],
    'linebreak-style': ['error', 'unix'],
    'no-trailing-spaces': ['error'],
    'no-unused-vars': [
      'error',
      {argsIgnorePattern: '^_', varsIgnorePattern: '^_'},
    ],
    quotes: ['error', 'single'],
    'object-curly-spacing': ['error', 'never'],
    semi: ['error', 'always'],
    'space-before-function-paren': ['error', 'never'],
    //-------------------------------------------------------------------------
    // https://github.com/yannickcr/eslint-plugin-react/tree/master/docs/rules
    //-------------------------------------------------------------------------
    'react/jsx-boolean-value': ['error', 'never'],
    'react/jsx-closing-bracket-location': 'error',
    'react/jsx-handler-names': ['warn', {eventHandlerPrefix: '_handle'}],
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
  },
};
