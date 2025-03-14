## Introduction
This repository provides an Erlang-based API for copy trading on Binance. It supports automatic switching between the K-lines (Candlesticks) of multiple digital currencies (cryptocurrency). When the mouse moves, a horizontal line will be displayed to represent the stop loss price. In addition, the take profit price will also be set when placing an order.

## Switching
When opening `mychart.html` in a browser, the price candlestick of the current currency will be displayed on the screen, and by default, it will switch to the next digital currency every 2 seconds:  

![img](https://github.com/binary2term/res/blob/main/1/1.gif)

## Periods
You can freely switch between different periods to display K-lines of different time spans:  

![img](https://github.com/binary2term/res/blob/main/1/2.gif)

## Zoom
You can press the 'F' key to zoom out the candlestick chart and the 'D' key to zoom in on the candlestick chart:  

![img](https://github.com/binary2term/res/blob/main/1/3.gif?raw=true)

## Configuration
Before starting the server, you must configure it. In the `src/ea1_api.erl` file, there are two fields corresponding to the secret key and api key you applied for on the binance exchange, as follows:
```
-define(SECRET_KEY, "your-binance-secretkey").
-define(API_KEY, "your-binance-apikey").
```
In the file `src/ea1_app.erl`, you can configure the port for the HTTP service:
```
Dispath = cowboy_router:compile([
{'_', [
  {"/chart", ea1_handler, []},
  {"/order1", ea1_handler, []},
  {"/order2", ea1_handler, []}
]}]),
{ok, _} = cowboy:start_clear(mylistener,
  [{port, 8088}],
  #{env => #{dispatch => Dispath}}
),
```
In `mychart.html`, you need to change the address in function `mousedown(x, y)` to your own:
```
var xhr = new XMLHttpRequest();
xhr.onreadystatechange = function () {
  if (xhr.readyState === 4) {
    output("xhr.rsp is "+xhr.response);
  }
};
xhr.open('post', 'http://your-server-ip:8088/order1', true);
```
And the same is true for function `updateChart()`:
```
xhr.open('post', 'http://your-server-ip:8088/chart', true);
xhr.setRequestHeader('Content-Type', 'application/x-www-form-urlencoded; charset=UTF-8');
xhr.send("symbol="+symbol+"&period="+period);
```
![img](https://github.com/binary2term/res/blob/main/1/4.gif)
