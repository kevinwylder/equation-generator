const path = require('path');
const HtmlWebpackPlugin = require('html-webpack-plugin');
const WasmPackPlugin = require("@wasm-tool/wasm-pack-plugin");

module.exports = {
    entry: './src/website.tsx',
    module: {
        rules: [
            {
                test: /\.tsx?$/,
                use: 'ts-loader',
                exclude: /node_modules/
            }
        ]
    },
    resolve: {
        extensions: [".tsx", ".ts", ".js"]
    },
    devtool: "eval-source-map",
    output: {
        path: path.resolve(__dirname, 'dist'),
        filename: 'index.js',
    },
    plugins: [
        new HtmlWebpackPlugin({
            "template": "src/index.html"
        }),
        new WasmPackPlugin({
            crateDirectory: path.resolve(__dirname, "."),
        }),
    ],
    mode: 'development',
    experiments: {
        asyncWebAssembly: true
    },
    performance: {
        hints: false,
        maxEntrypointSize: 1024 * 1024,
        maxAssetSize: 1024 * 1024,
    }

};

