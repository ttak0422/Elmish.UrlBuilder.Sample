const path = require('path');

module.exports = {
    entry: './src/Main.fsproj',
    output: {
        path: path.join(__dirname, './public'),
        filename: 'bundle.js'
    },
    devServer: {
        contentBase: './public',
        port: 8888
    },
    module: {
        rules: [{
            test: /\.fs(x|proj)?$/,
            use: 'fable-loader'
        }]
    }
}