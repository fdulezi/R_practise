x_PI = 3.14159265358979324 * 3000.0 / 180.0;
PI = 3.1415926535897932384626;
a = 6378245.0;
ee = 0.00669342162296594323;


bd09togcj02 = function(bd_lng, bd_lat){
    x = bd_lng - 0.0065;
    y = bd_lat - 0.006;
    z = sqrt(x * x + y * y) - 0.00002 * sin(y * x_PI);
    theta = atan2(y, x) - 0.000003 * cos(x * x_PI);
    gg_lng = z * cos(theta);
    gg_lat = z * sin(theta);
    return (c(gg_lng, gg_lat))
  };



transformlat = function(lng, lat){
  ret = -100.0 + 2.0 * lng + 3.0 * lat + 0.2 * lat * lat + 0.1 * lng * lat + 0.2 * sqrt(abs(lng))
  ret = ret+(20.0 * sin(6.0 * lng * PI) + 20.0 * sin(2.0 * lng * PI)) * 2.0 / 3.0
  ret = ret+ (20.0 * sin(lat * PI) + 40.0 * sin(lat / 3.0 * PI)) * 2.0 / 3.0
  ret = ret+ (160.0 * sin(lat / 12.0 * PI) + 320 * sin(lat * PI / 30.0)) * 2.0 / 3.0
  return (ret)
}

transformlng = function(lng, lat) {
  ret = 300.0 + lng + 2.0 * lat + 0.1 * lng * lng + 0.1 * lng * lat + 0.1 * sqrt(abs(lng));
  ret = ret+ (20.0 * sin(6.0 * lng * PI) + 20.0 * sin(2.0 * lng * PI)) * 2.0 / 3.0;
  ret = ret+ (20.0 * sin(lng * PI) + 40.0 * sin(lng / 3.0 * PI)) * 2.0 / 3.0;
  ret = ret+ (150.0 * sin(lng / 12.0 * PI) + 300.0 * sin(lng / 30.0 * PI)) * 2.0 / 3.0;
  return (ret)
}

gcj02towgs84 = function(lng, lat) {
    dlat = transformlat(lng - 105.0, lat - 35.0);
    dlng = transformlng(lng - 105.0, lat - 35.0);
    radlat = lat / 180.0 * PI;
    magic = sin(radlat);
    magic = 1 - ee * magic * magic;
    sqrtmagic = sqrt(magic);
    dlat = (dlat * 180.0) / ((a * (1 - ee)) / (magic * sqrtmagic) * PI);
    dlng = (dlng * 180.0) / (a / sqrtmagic * cos(radlat) * PI);
    mglat = lat + dlat;
    mglng = lng + dlng;
    return(c(lng * 2 - mglng, lat * 2 - mglat))
}

