
//=============================================================================//
// setting up
//=============================================================================//

//-----------------------------------------------------------------------------//
// renaming objects
//-----------------------------------------------------------------------------//

const map = this;
const LM = map.layerManager;

//-----------------------------------------------------------------------------//
// setting map click notification
//-----------------------------------------------------------------------------//

map.on("click", function() {console.log("---- map click ----")});

//-----------------------------------------------------------------------------//
// creating `sum` function to use with `reduce`
//-----------------------------------------------------------------------------//
                
function sum(total, num) {
    return total + num;
}

//-----------------------------------------------------------------------------//
// rewriting `getVisibleGroups` function
//-----------------------------------------------------------------------------//

// (b/c layerManager version wasn't working)

function getVisibleGroups() {
    
    // geting all group names
    
    var group_names = LM.getAllGroupNames();
    
    var group_is_on_map = [];
    
    // iterating over group names
    
    group_names.forEach(group_name => {
        
        var group_layers_on_map = [];
        
        // testing if each layer is on the map
        
        LM.getLayerGroup(group_name).eachLayer(layer => {
            
            // adding each layer's status to array w/ `push`
            
            group_layers_on_map.push(map.hasLayer(layer));
            
        }); 
        
        // if every layer in a group is on the map, then the group is on the map
        
        group_is_on_map.push(group_layers_on_map.every(has_layer => has_layer === true));
        
    });
    
    // getting element indexes of groups on map
    
    var on_map_group_index = group_is_on_map.reduce((a, e, i) => (e === true) ? a.concat(i) : a, []);
    
    // getting names of groups on map (iterating over index array, subsetting names array with index)
    
    var on_map_group_names = on_map_group_index.map(i => group_names[i]);
    
    // returning names of groups on mapo
    
    return on_map_group_names;
    
}


//-----------------------------------------------------------------------------//
// creating mouseover functions
//-----------------------------------------------------------------------------//

// but not setting mouseover style for the "highlight" circle marker, which 
//  is already highlighted, and needs to stay the same

function highlight(e) {
        
        if (!e.target.options.layerId.includes("highlight")) {
        
            e.target.setStyle({stroke: true, color: 'white', weight: 1, opacity: 1});
            e.target.setRadius(5);
            e.target.bringToFront();
        
        }
}

function unhighlight(e) {
        
        if (!e.target.options.layerId.includes("highlight")) {
        
            e.target.setStyle({stroke: false});
            e.target.setRadius(4);
            e.target.bringToBack();
        
        }
}

//-----------------------------------------------------------------------------//
// adding `div` overlays
//-----------------------------------------------------------------------------//

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - //
// station stats label
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - //

var station_stats_label = L.control({position: 'topright'});

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - //
// date label
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - //

// static for now, but will add more dates later

var date_label = L.control({position: 'topright'});

// defining date format for label

let date_format = new Intl.DateTimeFormat("en-US" , { year: "numeric", month: "long" });


//=============================================================================//
// adding events for station point interactions
//=============================================================================//

//-----------------------------------------------------------------------------//
// adding a reference to each layer in "all stations"
//-----------------------------------------------------------------------------//

var all_stations = [];

LM.getLayerGroup('all_stations').eachLayer(all_stations_layer => {
    
    all_stations.push(all_stations_layer);
    
});

// each time the map is clicked, the "visible group" gets recomputed
// the "mouseover" event here is just to make sure the process is initialized,
//  so that the below code handles the adding of every layer, including the "all stations"
//  layer added in R anbd already on the map when this code is evaluated

map.on("click mouseover", function() {
    
    // turn off mouseover
    
    map.off("mouseover");
    
    // get visible groups
    
    visible_group_name = getVisibleGroups();
    visible_group      = LM.getLayerGroup(visible_group_name);
    
    console.log("click map:", visible_group_name);

    // Everything in this section gets added to each layer (station point) in the visible group 
    
    visible_group.eachLayer(layer => {
        
        //-----------------------------------------------------------------------------//
        // highlighting station point on mouseover
        //-----------------------------------------------------------------------------//
        
        //---- adding highlight ----//
        
        layer.on('mouseover', e => highlight(e));
        
        
        //---- removing highlight ----//
        
        layer.on('mouseout', e => unhighlight(e));
        
        
        //-----------------------------------------------------------------------------//
        // showing station group on click
        //-----------------------------------------------------------------------------//
        
        layer.on('click', function (e) {
            
            //---- tracking layer click ----//
            
            console.log("---- layer click ----");
            
            
            //---- identifying clicked station by layerId ----//
            
            selected_station_name = e.target.options.layerId;
            
            console.log("selected_station_name:", selected_station_name);
            
            
            //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - //
            // setting data filtering parameters
            //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - //        
            
            // hard-coded for now; will eventually make this interactive
            
            const year = "2019";
            const month = "4";
            
            
            //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - //
            // Adding end station points
            //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - //        
            
            //---- if the selected station is clicked again, don't remove it ----//
            
            if (visible_group_name != selected_station_name) {
                
                // getting data from the selected station
                
                var station_data = data[year][month][selected_station_name];
                
                // creating variable to store layers
                
                var station_point_layers = [];
                var added_log = [];
    
                //---- looping through all rows in data, adding all end stations ----//
                
                for (var i = 0; i < station_data.trips.length; i++) {
                    
                    // setting up log
                    
                    added_log[i] = station_data.end_station_name[i];
                    
                    // creating circleMarker layer w/ tooltip
                    
                    station_point_layers[i] = 
                        new L.circleMarker(
                                [
                                    station_data.end_station_latitude[i],
                                    station_data.end_station_longitude[i]
                                ], 
                                {
                                    radius: 4,
                                    stroke: false,
                                    fill: true,
                                    fillColor: station_data.colors[i],
                                    fillOpacity: 1
                                }
                            )
                            .bindTooltip(
                                station_data.end_station_name[i] + 
                                " - " + 
                                station_data.trips[i] + 
                                " trips",
                                {textsize: '1.1em', direction: 'top', offset: [0, -10]}
                            );
                    
                    // adding layer through layerManager, to add groupname
                    
                    LM.addLayer(
                        station_point_layers[i],
                        "marker",
                        station_data.end_station_name[i],
                        selected_station_name,
                        null,
                        null
                    );
                    
                    // manually adding layerId
                    
                    station_point_layers[i].options.layerId = station_data.end_station_name[i];
                
                }
                
                                
                //---- adding large circle marker at click point ----//
                
                // creating circleMarker layer
                
                station_point_highlight = 
                    new L.circleMarker(
                            e.latlng, 
                            {
                                radius: 8,
                                stroke: true,
                                weight: 2,
                                fill: false,
                                color: 'white',
                                opacity: 1
                            }
                        ).bringToFront();
                
                
                // adding layer through layerManager, to add groupname
                
                LM.addLayer(
                    station_point_highlight,
                    "marker",
                    selected_station_name + " highlight",
                    selected_station_name,
                    null,
                    null
                );             
                
                // manually adding layerId
                
                station_point_highlight.options.layerId = selected_station_name + " highlight";
                
                
                //---- changing tooltip text size ----//
                
                console.log("click added:", selected_station_name, added_log);
                console.log("post-add map:", getVisibleGroups());
                
                
                //-----------------------------------------------------------------------------//
                // label overlays
                //-----------------------------------------------------------------------------//
                
                //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - //
                // showing station group stats on the map
                //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - //
                
                //---- getting total trips count for selected station ----//
                
                var total_trips = station_data.trips.reduce(sum, 0);
                
                
                //---- adding box showing total trips count for selected station ----//
                
                station_stats_label.onAdd = function (map) {
                    
                    // setting class and additional styles
                    
                    var div = L.DomUtil.create('div', 'leaflet-control-layers');
                    
                    div.style.setProperty("padding", "5px 10px 5px 10px");
                    
                    div.style.setProperty("text-align", "center");
                    
                    // creating label box
                    
                    div.innerHTML = 
                        "<span style = 'font-family: sans-serif; font-size: 1.5em;" +
                        " font-weight: bold;'>" + 
                        selected_station_name + 
                        "</span>" + 
                        "<br>" +
                        "<span style = 'font-family: sans-serif; font-size: 1.3em;'>" + 
                        total_trips.toLocaleString() +
                        " total trips" +
                        "</span>";
                
                    L.DomEvent.disableClickPropagation(div);
                    
                    return div;
                    
                };
                
                station_stats_label.addTo(map);
                
        
                //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - //
                // showing observation date on the map
                //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - //
                                
                //---- transforming year + month into date ----//
                
                // I will eventually add an actual date filtering control, but for now, I'll just 
                //  filter based on station name
                
                const date = new Date(year, month-1); // month is 0-indexed
                
                date_label.onAdd = function (map) {
                    
                    // setting class and additional styles
                    
                    var div = L.DomUtil.create('div', 'leaflet-control-layers');
                    
                    div.style.setProperty("padding", "3px 7px 3px 7px");
                    
                    div.style.setProperty("text-align", "center");
                    
                    // creating label box
                    
                    div.innerHTML = 
                        "<span style = 'font-family: sans-serif; font-size: 1.3em;" +
                        " font-weight: bold;'>" + 
                        date_format.format(date) + 
                        "</span>";
                
                    L.DomEvent.disableClickPropagation(div);
                    
                    return div;
                    
                };
                
                date_label.addTo(map);
                
    
                //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - //
                // removing previous station group
                //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - //                
                
                // Visible group was defined above on map click
                    
                var removed_log = [];
                
                visible_group.eachLayer(layer => {
                    
                    removed_log.push(layer.options.layerId);
                    
                    // correcting for sticky mouseover style changes
                    
                    if (!layer.options.layerId[0].includes("highlight")) {
                        
                        layer.setStyle({stroke: false});
                        layer.setRadius(4);
                        layer.bringToBack();
                        
                    }                        
                    
                    layer.remove();
                    
                });
                
                // removing sticky "all stations" layer points
                
                all_stations.forEach(all_stations_layer => {
                    
                    all_stations_layer.remove();
                    
                });                
                
                console.log("click removed:", visible_group_name, removed_log);
                
                
            }
            
            // this should return value of `selected_station_name`
            
            console.log("final map:", getVisibleGroups());
            
        });        
        
    }); 
}); 


//=============================================================================//
// adding easyButtons
//=============================================================================//

//-----------------------------------------------------------------------------//
// easyButton to go back to the "all stations" group//-----------------------------------------------------------------------------//

L.easyButton({
        position: "bottomleft",
        states: [{
            title: "Reset Layer",
            icon: "fas fa-undo",
            
            onClick: function() {
                
                // removing labels
                
                station_stats_label.remove();
                date_label.remove();
                
                // removing each layer group
                
                console.log("EB pre-remove", getVisibleGroups());
                
                var visible_group_names = getVisibleGroups();
                
                visible_group_names.forEach(visible_group_name => {
                    
                    var removed_log = [];
                    
                    LM.getLayerGroup(visible_group_name).eachLayer(visible_layer => { 
                        
                        removed_log.push(visible_layer.options.layerId);
                        
                        visible_layer.remove();
                        
                    });
                    
                    console.log("EB removed:", visible_group_name, removed_log);
                    
                    
                });
                
                // removing sticky "all stations" layer points
                
                all_stations.forEach(all_stations_layer => {
                    
                    all_stations_layer.remove();
                    
                });
                
                console.log("EB post-remove", getVisibleGroups());
                
                
                //  adding the "all stations" layer
                
                var added_log = [];
                
                all_stations.forEach(all_stations_layer => {
                    
                    added_log.push(all_stations_layer.options.layerId);
                    all_stations_layer.addTo(map);
                    
                    // resetting the previuously clicked point to the default properties
                
                    all_stations_layer.setStyle({stroke: false});
                    all_stations_layer.setRadius(4);
                    
                });
                
                console.log("EB added:", "all_stations", added_log);
                console.log("EB post-add", getVisibleGroups());
                
                // firing map click event, so that "visible group" gets refreshed
                
                console.log("---- EB fire map click ----");
                map.fire("click");
                
            }
        }]
}).addTo(map);


//-----------------------------------------------------------------------------//
// easyButton to zoom to end station points
//-----------------------------------------------------------------------------//

L.easyButton({
        position: "bottomleft",
        states: [{
            title: "Zoom to Layer",
            icon: "fas fa-circle",
            
            onClick: function() {
                
                map.fitBounds(
                    [
                        LM.getLayerGroup(getVisibleGroups()).getBounds()
                    ], 
                    {"padding": [5, 5], "duration": 0.5, "zoomSnap": 0.5}
                );
                
            }
        }]
}).addTo(map);


// ----------------------------- THIS IS THE END! ---------------------------- //
