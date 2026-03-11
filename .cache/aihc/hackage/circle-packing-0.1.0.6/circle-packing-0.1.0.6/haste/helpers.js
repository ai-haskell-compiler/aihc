function jsGetContext2D(canvas, _) {
	return canvas.getContext("2d");
}

function jsBeginPath(context, _) {
	context.beginPath();
}

function jsClosePath(context, _) {
	context.closePath();
}

function jsFill(context, _) {
	context.fill();
}

function jsArc(context, x, y, r, minPhi, maxPhi, ccw, _) {
	context.arc(x,y,r,minPhi,maxPhi,ccw);
}

function jsSetFillColor(context, color, _) {
	context.fillStyle = color;
}

function jsClear(context, _) {
	context.clearRect(0.0, 0.0, context.canvas.width, context.canvas.height);
}

