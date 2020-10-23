---
pagetitle: NYC Citi Bike trips
---

<!-- I'm specifying these basic HTML properties here in the markdown doc. This will force the map to render as full screen with no borders. I'm letting RStudio's conversion to HTML take care of the actual rendering. Magic! -->  
  
<meta name="viewport" content="width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no" />
  
<head>
    <style>
      body {
          padding: 0;
          margin: 0;
      }
      html, body, #map {
          height: 100%;
          width: 100%;
          position: absolute;
          top: 0px;
          bottom: 0px;
          left: 0px;
          right: 0px;
      }
    </style>
</head>

<iframe id="map" src="stations_map.html" style="border:none; background:none"> </iframe>
