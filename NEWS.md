# VOSONDash 0.5.6

## Minor Changes:
- Set `visNetwork` plot edge width if weight column present in edges.
- Removed arrows from `visNetwork` plot if graph undirected.
- Fixed a groups parameter warning from `dplyr::summarise`. 
- Cleaned up package dependencies.
- CRAN version update.

# VOSONDash 0.5.5

## Bug Fixes:
- Fixed an issue with custom classes assigned to dataframes causing an vctrs error when using dplyr functions. The classes are not required so they are simply removed.

# VOSONDash 0.5.4

## Minor Changes:
- Removed code support for older `vosonSML` versions prior to `0.29`.
- Code refactoring and removal of `networkD3` plots.
- Added new hex sticker to package documentation.

# VOSONDash 0.5.3

## Minor Changes:
- Added the option to inherit node colors from loaded `graphml` files in plots.
- Clicking nodes in the `visnetwork` plot now also selects or deselects them in the data table below it.
- Added a more simple node label option of `node index` which is the numeric index of the vertex.
- Added the `niter` or "number of iterations" input field for `FR` and `GraphOpt` plots.
- Added the `charge`, `mass`, `spring.length` and `spring.constant` input fields for `GraphOpt` plots.

<br/>Refer to `igraph` documentation for further information on layout parameters at https://igraph.org/r/doc/layout_with_fr.html and https://igraph.org/r/doc/layout_with_graphopt.html.

# VOSONDash 0.5.2

## Minor Changes:
- Moved `rtweet` from package imports to suggests.

## Bug Fixes:
- Fixed `reddit` URL formatting to support retrieving additional threads.

# VOSONDash 0.5.1

## Minor Changes:
- Renamed `bimodal` networks to `twomode`.
- Added percentage sliders to twitter `semantic` network creation tab.

# VOSONDash 0.5.0

## Major Changes:
- Supports the new `vosonSML` version `0.29` changes to the network and graph creation process. It is backwards compatable with version `0.27` albeit without the new features.
- Added a new tab to the `Collect Data` controls named `Create Network`. This allows the creation of the different types of networks that are supported by `vosonSML`.

#### Twitter:
- Networks supported are `activity`, `actor`, `bimodal` and `semantic`.
- Both `activity` and `actor` have an option to `Add Text` that adds tweet text as a network node or edge attribute.
- The `actor` network also has the option to `Lookup User Data`. This will retrieve profile information for users that became nodes during network creation who were not authors of tweets. Their profile information was missing most likely because they were referenced in tweets but none of their tweets were collected in the search. In a twitter search profile information is only returned for the authors of tweets captured in the search.
- Both `bimodal` and `semantic` networks have the option to filter out terms using the `Remove Terms` field. This accepts a comma delimited list of terms that can be `actors` and `hashtags` (e.g \@climate_person, #climate) for `bimodal` networks and `terms`, `actors` and `hashtags` (e.g climate, \@climate_person, #climate) for `semantic` networks. The `semantic` network currently uses the default `vosonSML` options of only including the 5% most frequent terms and 50% most frequently occuring hashtags in the network. 

#### Youtube:
- Networks supported are `activity` and `actor`.
- Both `activity` and `actor` have an option to `Add Text` that adds video comment text as a network node or edge attribute.
- The `actor` network with `Add Text` option allows for the further option to `Find Replies in Text`. This will create reply `edges` between `actors` in the network when an `@actor_name` reference is found at the beginning of a text comment.
- There is also the `Add Video Details` option for `actor` networks that retrieves video information and adds it to the network as a `node` attributes of `VideoID` type nodes (e.g title, description and publisher). This also replaces `VideoID` type nodes with the actors ID who published the video.
- A further option for `Add Video Details` is `Only replace Video ID's`. This option does not supplement the network with the additional video information retrieved but instead only replaces the `VideoID` type nodes with the actors ID who published the video.

#### Reddit:
- Networks supported are `activity` and `actor`.
- Both `activity` and `actor` have an option to `Add Text` that adds thread comment text as a network node or edge attribute. 

<br/>Refer to `vosonSML` documentation for further information on network types and options at https://vosonlab.github.io/vosonSML/reference/index.html.

## Minor Changes:
- Added a new `Collect` data download button named `Network` to allow the `nodes` and `edges` dataframes to be downloaded after `Create Network`. The data is downloaded as an R object in a `.rds` file that can be loaded into R using the `readRDS()` function. 

## Bug Fixes:
- Fixed the console scrolling in the `Collect` section of the interface to scroll to the bottom when there is new text output.

# VOSONDash 0.4.4

## Minor Changes:
- Changed the required package check to be more efficient by calling `required()` instead of `installed.packages()`.
- Changed the app startup messages to use `message()` instead of `cat()` so if desired they can be suppressed.
- Added `pkgStartupMsgs` parameter to `runVOSONDash()` to hide the app's required package startup messages.
- Added optional `isLocal` parameter to `runVOSONDash()` to manually set local or server flag if desired. If not set app will detect if it is running in server mode.
- Disabled the loading and saving of keys and tokens in server mode.
- Added a tooltip to `Create Web Auth Token` to add some further information on process and note.

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