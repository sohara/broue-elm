const autoprefixer = require('autoprefixer');
const tailwindcss = require('tailwindcss');

module.exports = {
  configureWebpack: (config, env) => {
    // Manipulate the config object and return it.
    console.log(config.module.rules);
    config.module.rules = config.module.rules.map(rule => {
      if (rule.test && rule.test.toString().includes('css')) {
        console.log("!!!!!!FOUND RULE: " + rule.test.toString() );
        // return rule;
        // Replace the entire config
        return {
          test: /\.css/,
          use: [
            require.resolve('style-loader'),
            {
              loader: require.resolve('css-loader'),
              options: {
                importLoaders: 1
              }
            },
            {
              // Options for PostCSS as we reference these options twice
              // Adds vendor prefixing based on your specified browser support in
              // package.json
              loader: require.resolve('postcss-loader'),
              options: {
                // Necessary for external CSS imports to work
                // https://github.com/facebook/create-react-app/issues/2677
                ident: 'postcss',
                plugins: () => [
                  require('postcss-flexbugs-fixes'),
                  autoprefixer({
                    flexbox: 'no-2009'
                  }),
                  tailwindcss('./tailwind.js')
                ]
              }
            }
          ]
        }
      }
      return rule;
    });
    // throw new Error("What?");
    return config;
  }
}
