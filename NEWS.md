# VOSONDash 0.4.4

## Minor Changes:
- Changed the required package check to be more efficient by calling `required()` instead of `installed.packages()`.
- Changed the app startup messages to use `message()` instead of `cat()` so if desired they can be suppressed.

# VOSONDash 0.4.3

## Minor Changes:
- Changed the node size controls in `Network Graphs` to be based on normalized continuous values.
- Moved the calculation of network metrics to the `getNetworkMetrics()` package function.
- Demonstration data select box now hidden when another data source is loaded.
- Abbreviated long name layouts in the `Graph layout` control.
- Added tooltips to various interface controls using the `popper.js` javascript library (v1.14.3). Documentation at https://getbootstrap.com/docs/4.1/components/popovers/ and https://popper.js.org.

# VOSONDash 0.4.2

## Bug Fixes:
- Fixed problem with `visNetwork` graphs rendering slightly off the canvas. 

## Major Changes:
- `VOSONDash` Shiny app has been re-structured into an R package. Have started moving some of the 'business logic' away from the app server and into the general package.
- Social Media API keys are no longer saved to the users working directory. API keys and tokens are now saved to files in the users home directory as specified by the system environment variable `HOME`. This location can be found in the `VOSONDash` start up information, in the apps `API Keys` tab or by using the following R function in `RStudio`: `Sys.getenv("HOME")`. The keys and tokens files are named `vosondash_keys.rds` and `vosondash_tokens.rds` respectively.
- Twitter no longer creates an API authentication token for each twitter collection. Instead twitter tokens are now created and managed in the `API Keys` tab and should be saved and re-used.
- A twitter web authorization token can now be created and used for API access without the need for a Developer account. This still requires a twitter app and Consumer API keys but means if a user has access to these that the user can authorize the twitter app against their account and use `VOSONDash` to perform twitter collections. Using this token API rate-limiting will be applied to the users account not the app. Twitter refers to this method of authentication as 'Application-user authentication: OAuth 1a (access token for user context)'. This feature uses the `rtweet` packages interactive web authorization as implemented by the `rtweet::create_token` function. As stated above once a token has been created it should be saved and re-used.

## Minor Changes:
- Demonstration data has been included in the package and is accessible by a new selection box below the graphml file loader in `Network Graphs`. If there is a file with the same name as the demonstration graphml file but with an additional '.txt' extension in the packages demonstration data folder it will be loaded as the graph description.
- Added graph summary information for `Network Graphs` plots as an overlay in the bottom left corner.
- Added a select box in the top right corner of the graph plots that the user can use to select the render plot height in pixels.
- Changed selected row and vertex highlighting colors in graph data table and plot.
- Removed `visNetwork` plot navigation controls.
- Removed `D3` network plots for the time being.
- NRC sentiment analysis plots have been re-worked and now split into the 8 emotions, measured and sorted as the proportion of the total of the emotions in the text and the valence which is the measure of how positive or negative the text is.
- The `All categories` plot has now been removed from text analysis when single categories are chosen or combined. In that particular use-case the plots are the same. It will only be shown when the `All` value in the sub-categories select box is selected.
- Removed keys fields from `Collect` twitter and it now displays selected auth token. 
- Variable naming changes and a lot of code re-structuring and refactoring. 