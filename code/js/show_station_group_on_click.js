
//=============================================================================//
// setting up
//=============================================================================//

//-----------------------------------------------------------------------------//
// renaming objects
//-----------------------------------------------------------------------------//

const map = this;
const LM = map.layerManager;

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

// Everything in this section gets added to each layer (point) in each station group 

LM.getAllGroupNames().forEach(group_name => {
    
    var group = LM.getLayerGroup(group_name);
    
    group.eachLayer(layer => {
        
        //-----------------------------------------------------------------------------//
        // highlighting station point on mouseover
        //-----------------------------------------------------------------------------//
        
        //---- adding highlight ----//
        
        layer.on('mouseover', function () {
            
            // but not setting mouseover style for the "highlight" circle marker, which 
            //  is already highlighted, and needs to stay the same
            
            if (!layer.options.layerId.includes("highlight")) {
            
                layer.setStyle({stroke: true, color: 'white', weight: 1, opacity: 1});
                layer.setRadius(5);
                layer.bringToFront();
            
            }
    
        });
        
        
        //---- removing highlight ----//
    
        layer.on('mouseout', function () {
            
            // see above re: highlight
            
            if (!layer.options.layerId.includes("highlight")) {
            
                layer.setStyle({stroke: false});
                layer.setRadius(4);
                layer.bringToBack();
            
            }
        });
        
        
        //-----------------------------------------------------------------------------//
        // showing station group on click
        //-----------------------------------------------------------------------------//
        
        layer.on('click', function (e) {
            
            
            //---- tracking layer click ----//
            
            console.log("---- layer click ----");
            
            
            //---- identifying clicked station by layerId ----//
            
            var selected_station = e.target.options.layerId;
            
            // parsing layerId's with "_" to get station name
            
            if (selected_station.includes("_")) {
                
                selected_station_layerId = e.target.options.layerId;
                selected_station_id      = selected_station_layerId.split("_")[1];
                selected_station_index   = data.start_station_id.indexOf(selected_station_id);
                selected_station_name    = data.start_station_name[selected_station_index];
                
            } else {
                
                // these layerId's are station names
                
                selected_station_name = e.target.options.layerId;
                
            }
            
            console.log("selected_station_name:", selected_station_name);
            
            
            //---- if the selected station is clicked again, do no adding or removing ----//
            
            if (getVisibleGroups() != selected_station_name) {
                
                
                //---- adding selected station's end stations layer ----//
                
                var added_log = [];
                
                LM.getLayerGroup(selected_station_name).eachLayer(selected_station_layer => { 
                    
                    added_log.push(selected_station_layer.options.layerId);
                    selected_station_layer.addTo(map);
                    
                });
                
                console.log("click added:", selected_station_name, added_log);
                console.log("post-add map:", getVisibleGroups());
                
                
                //-----------------------------------------------------------------------------//
                // label overlays
                //-----------------------------------------------------------------------------//
                
                //---- getting total trips count for selected station ----//
                
                var station_index = data.start_station_name.indexOf(selected_station_name);
                
                //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - //
                // showing station group stats on the map
                //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - //
                
                //---- getting total trips count for selected station ----//
                
                var total_trips = data.trips[station_index];
                
                
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
                
                // I will eventually add actual date filtering, but for now, I'll just filter
                //  based on station name
                
                const date = new Date(data.year[station_index], data.month[station_index]-1);                
                
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
                
                // getting all visible groups
                
                var visible_group_names = getVisibleGroups();
                
                // visible groups that are not the selected group
                
                var groups_to_remove = visible_group_names.filter(group => group != selected_station_name);
                
                console.log("groups to remove:", groups_to_remove);
                    
                groups_to_remove.forEach(group_name => {
                    
                    var removed_log = [];
                    
                    LM.getLayerGroup(group_name).eachLayer(layer => { 
                        
                        removed_log.push(layer.options.layerId);
                        
                        // correcting for sticky mouseover style changes
                        
                        if (!layer.options.layerId.includes("highlight")) {
                        
                            layer.setStyle({stroke: false});
                            layer.setRadius(4);
                            layer.bringToBack();
                        
                        }                        
                        
                        layer.remove();
                        
                    });  
                    
                    console.log("click removed:", group_name, removed_log);
                    
                });
    
            }        
            
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
                
                // removing each layer group, 
                
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
                
                console.log("EB post-remove", getVisibleGroups());
                
                // then adding the "all stations" layer
                
                var added_log = [];
                
                LM.getLayerGroup('all_stations').eachLayer(all_stations_layer => { 
                    
                    added_log.push(all_stations_layer.options.layerId);
                    all_stations_layer.addTo(map);
                    
                    // resetting the previuously clicked point to the default properties
                
                    all_stations_layer.setStyle({stroke: false});
                    all_stations_layer.setRadius(4);
                    
                });
                    
                
                console.log("EB added:", "all_stations", added_log);
                console.log("EB post-add", getVisibleGroups());
                
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
