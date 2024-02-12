# ArXiv Retriever

An open source TUI application to retrieve papers from the arXiv using its public API. The source code in this directory compiles with `cabal` to an executable that can be run from the command line. Default execution options can be edited in the file in the `app` directory entitled `Config.hs`. Information on these options can be found in the file.

## Options

### Search field and query
Several options are available on the command line. A basic search on the arXiv can be performed by running the file with a search query in a particular search field. For example, to search article titles for the string `"Interaction"`, add the `-ti Interaction` option when running the executable. Similarly, to search paper abstracts for the string `"Water"`, simply add the `-abs Water` option. It is worth noting that only one search can be performed at a time, so `-ti Interaction -abs Water` would not yield the desired results. By default, this option is set to `-cat cs.CL`, which searches for articles within the category of Computation and Language. To change the defaults, edit the `Config.hs` file in the `app` directory. Instructions are in the file itself. The options for search field are as follows:
- `ti`: search within titles of articles.
- `au`: search within authors of articles.
- `abs`: search within article summaries.
- `co`: search within article comments.
- `jr`: search within journal reference.
- `cat`: search for a category (DEFAULT).
- `rn`: search for a reference number.
- `id`: search for paper ID.
- `all`: search in all fields.

### Beginning value
Another querying option is the beginning value. This is the (zero-indexed) index of the first paper to be returned by the query. By default this is set to 0, but can be changed to e.g. 1 by adding the `-b 1` option to the query. To change the defaults, edit the `Config.hs` file in the `app` directory. Instructions are in the file itself.

### Maximum returned
The maximum returned option gives the maximum number of papers to be returned by the API call. By default this is set to 10, but can be changes to e.g. 20 by adding the `-m 20` option to the query. To change the defaults, edit the `Config.hs` file in the `app` directory. Instructions are in the file itself.

### Sort key
The sort key is what the arXiv API uses to sort the results it returns. By default this is set to submission date, but can be changed to e.g. relevance by adding the `-s rel` option to the query. To change the defaults, edit the `Config.hs` file in the `app` directory. Instructions are in the file itself. The following are the options for sort key values:
- `submitted`: submission date (DEFAULT).
- `updated`: latest updating date.
- `rel`: relevance.

### Sort order
The sort order simply tells the API whether to sort by the above key in ascending or descending order. By default this is set to descending, but can be changed to ascending by adding the `-asc` option to the query, and back to descending by adding a `-desc` option instead.

## Project information
This project was started by [Nicolas Antonio Cloutier](mailto:nicocloutier1@gmail.com) in 2024. There are no additional contributors as of yet. If you have suggestions, issues, or additions, feel free to open an issue or pull request on the [GitHub page](https://github.com/NicoACloutier/arxiv_ret). This project operates under the MIT license. Licensing information can be found in the file entitled `LICENSE` in this project's top-level directory.

## Special thanks
Thank you to arXiv for use of its open access interoperability.