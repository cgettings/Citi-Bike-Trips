var url ='https://ridewithgps.com/find#search/0/search[offset]=75&search[start_distance]=1&search[start_location]=40.8428954299466,+-73.8505409271907&search[keywords]=&search[length_min]=0&search[length_max]=20&search[elevation_min]=0&search[elevation_max]=10000&search[recreation_types][]=3&search[recreation_types][]=4&search[recreation_types][]=10&search[sort_by]=length+asc';
    var page = new WebPage();
    var fs = require('fs');

    page.open(url, function (status) {
            just_wait();
    });

    function just_wait() {
        setTimeout(function() {
                   fs.write('pages/results.html', page.content, 'w');
                phantom.exit();
        }, 2500);
    }
    
