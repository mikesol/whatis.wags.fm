const path = require("path");
const webpack = require("webpack");
const CopyPlugin = require("copy-webpack-plugin");
module.exports = {
	mode: "development",
	entry: "./src/index.js",
	output: {
		path: path.resolve(__dirname, "dist"),
		filename: "bundle.js",
	},
	module: {
		rules: [
			{
				test: /\.js$/i,
				include: path.resolve(__dirname, "src"),
				use: {
					loader: "babel-loader",
					options: {
						presets: ["@babel/preset-env"],
					},
				},
			},
			{
				test: /\.s[ac]ss$/i,
				include: [
					path.resolve(__dirname, "src"),
					path.resolve(__dirname, "css"),
				],
				use: ["style-loader", "css-loader", "sass-loader"],
			},
			{
				test: /\.css$/i,
				include: [
					path.resolve(__dirname, "src"),
					path.resolve(__dirname, "css"),
					path.resolve(__dirname, "plugin"),
					path.resolve(__dirname, "node_modules/notyf"),
					path.resolve(__dirname, "node_modules/@fortawesome/fontawesome-free"),
				],
				use: ["style-loader", "css-loader"],
			},
		],
	},
	resolve: {
		fallback: { url: require.resolve("url/") },
	},
	plugins: [
		new webpack.IgnorePlugin({
			resourceRegExp: /^xhr2$/,
		}),
		new CopyPlugin({
			patterns: [{ from: path.resolve(__dirname, "src/images"), to: "images" }],
		}),
	],
	devServer: {
		static: {
			directory: path.join(__dirname, "dist"),
		},
	},
};
