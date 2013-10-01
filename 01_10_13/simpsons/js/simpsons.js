$(document).ready(function(){
	make_movable('#scroll-couch');
	make_movable('#scroll-marge');
	make_movable('#scroll-homer');
	make_movable('#scroll-lisa');
	make_movable('#scroll-bart');
	make_movable('#scroll-little');	
});

function make_movable(div_name){
	$(div_name).smart3d({
		vertical: false 
	});	
}