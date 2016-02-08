eSTAR Development
======================================

This is the core repository for the eSTAR site. The eSTAR aim to develop an Analytic Took Kit to find alpha and create a custom IDE based on R-Shiny.
 
#### Project Status: *ALPHA*

During the alpha stage, expect many things to break, not work or simply fail.

#### Documentation ####

We have several documentation resources for you:

<table>
    <tr><th>API documentation</th><td>http://eSTAR.beyondbond.com/docs/eSTAR-api</td></tr>
    <tr><th>User documentation</th><td>http://eSTAR.beyondbond.com/docs/eSTAR-user</td></tr>
</table>

Please join the mailinglist to get support or give support to the growing community of plugin developers:
https://groups.google.com/forum/#!forum/beyondbond-eSTAR

#### Installation ####

Follow these steps to install the eSTAR:

    git clone https://github.com/beyondbond/eSTAR.git
    
#### Starting eSTAR ####

Start the eSTAR as follows:

    node server.js

The following options can be used:

    --settings       Settings file to use
    --help           Show command line options.
    -t               Start in test mode
    --port           Port
    --debug          Turn debugging on
    --listen         IP address of the server
    --auth           Basic Auth username:password

Now visit [http://localhost:8181/](http://localhost:8181/) to load eSTAR.

#### Running Tests ####

Run server side tests with:
    
    npm run test
    
Run client side tests with:

    npm run ctest
    
Then visit [http://localhost:8181/static/test](http://localhost:8181/static/test) in your browser.

#### Contributing ####

We actively encourage and support contributions. We accept pull requests to the core as well as to any of the open source plugins and libraries that we maintain under the Beyondbond, Inc. organization on GitHub.

Feel free to fork and improve the eSTAR and open a pull request. For more information on our contributing guidelines, see our contributing guide: http://eSTAR.beyondbond/docs/contributing-to-eSTAR


If you want to contribute to the eSTAR please go to the online form, fill it out and submit it.

Happy coding, Beyondbond
