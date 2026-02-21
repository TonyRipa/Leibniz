
/*
	Author:	Anthony John Ripa
	Date:	2/20/2026
	View:	A view library
*/

class View {

	static me2parkid(dag,me) {
		let par = dag.par[me]
		let kid = dag.kid[me]
		return {par,kid}
	}

	static makeinputbig($cont,dag,me,data) {
		let {par,kid} = View.me2parkid(dag,me)
		$cont.append(`<textarea id='${me}' cols='180' rows='7'>${data}</textarea>`)
		return ()=>{if (par) putval(me,$('#'+par).val())}
	}

	static makeselect($cont,dag,me,data) {
		let {par,kid} = View.me2parkid(dag,me)
		if (Array.isArray(data))
			data = data.map(d=>'<option>'+d+'</option>')
		else
			data = Object.keys(data).map(key=>`<optgroup label='${key}'>`+data[key].map(d=>'<option>'+d+'</option>'))
		$cont.append(`<select id='${me}'>${data}</select>`)
		return ()=>{}
	}

	static makeinput($cont,dag,me,data='') {
		let {par,kid} = View.me2parkid(dag,me)
		$cont.append(`<input id='${me}' value='${data}' placeholder='${me}'>`)
		return ()=>{if (par) $('#'+me).val($('#'+par).val())}
	}

	static makefilter($cont,dag,me) {
		let {par,kid} = View.me2parkid(dag,me)
		$cont.append(`<textarea id='${me}' cols='50' rows='7' placeholder='${me}'></textarea>`)
		return ()=>set_textarea(me,Stats.p(get_input(par[0]),id2array(par[1])))
	}

	static makeprolog($cont,dag,me) {
		let {par,kid} = View.me2parkid(dag,me)
		$cont.append(`<textarea id='${me}' cols='50' rows='7' placeholder='${me}'></textarea>`)
		return () => {prolog.do(par[0],par[1],me)}
	}

	static makewhere($cont,dag,me) {
		let {par,kid} = View.me2parkid(dag,me)
		$cont.append(`<textarea id='${me}' cols='150' rows='7' placeholder='${me}'></textarea>`)
		return ()=>set_textarea(me,Frame.fromstr(get_input(par[1])).where(get_input(par[0])))
	}

	static makeoddschain2oddstable($cont,dag,me) {
		let {par,kid} = View.me2parkid(dag,me)
		$cont.append(`<textarea id='${me}' cols='50' rows='10' placeholder='${me}'></textarea>`)
		return ()=>{
			let r = id2array(par,',')[0]
			let ret = Stats.oddschain2oddstable(r)
			set_textarea(me,ret)
		}
	}

	static makeprob2oddstable($cont,dag,me) {
		let {par,kid} = View.me2parkid(dag,me)
		$cont.append(`<textarea id='${me}' cols='30' rows='10' placeholder='${me}'></textarea>`)
		return ()=>{
			let ret = ''
			let p = id2array(par,',')[0]
			for (let i = 0 ; i < p.length ; i++) {
				for (let j = 0 ; j < p.length ; j++) {
					let odds = p[i] / p[j]
					if (isNaN(odds)) odds =  '%'
					if (odds == 1/0) odds =  '∞'
					if (odds ==-1/0) odds = '-∞'
					ret += odds
					if (j<p.length-1) ret += '\t'
				}
				if (i<p.length-1) ret += '\n'
			}
			set_textarea(me,ret)
		}
	}

	static makeplot($cont,dag,me) {
		let {par,kid} = View.me2parkid(dag,me)
		$cont.append(`<div id='${me}' style='border:thin solid black;width:100px;height:50px;color:#999'>${me}</div>`)
		return () => {
			let frame = Frame.fromString(get_input(par))
			$('#'+me).empty()
			$('#'+me).removeAttr('style')
			if (frame.numcols()==2) Plot.fromFrame(frame).plot1 (me)
			if (frame.numcols()==3) Plot.fromFrame(frame).table2(me)
			if (frame.numcols()==4) Plot.fromFrame(frame).table3(me)
		}
	}

	static makenetwork($cont,dag,me) {
		let {par,kid} = View.me2parkid(dag,me)
		window.godiagram = null
		$cont.append(`<div id='${me}' style='border:thin solid black;width:640px;height:400px;color:#999'>${me}</div>`)
		return () => Plot.plotnetwork(me,get_input(par))
	}

	static makejson2net($cont,dag,me) {
		let {par,kid} = View.me2parkid(dag,me)
		window.godiagram = null
		$cont.append(`<div id='${me}' style='border:thin solid black;width:640px;height:400px;color:#999'>${me}</div>`)
		return () => Plot.json2net(me,get_input(par))
	}

	static makeplots($cont,dag,me) {
		let {par,kid} = View.me2parkid(dag,me)
		$cont.append(`<div id='${me}' style='border:thin solid black;width:100px;height:50px;color:#999'>${me}</div>`)
		return () => {
			let frame = Frame.fromString(get_input(par))
			let frame1 = frame.copy().removecol(1)
			let frame2 = frame.copy().removecol(0)
			if (frame.numcols() >= 3) {
				$('#'+me).empty()
				$('#'+me).removeAttr('style')
				$('#'+me).append(`<table><tr><td id='${me}1' width='500px'></td><td id='${me}2' width='500px'></td></tr></table>`)
				Plot.fromFrame(frame1).plot1(me+1)
				Plot.fromFrame(frame2).plot1(me+2)
			}			
		}
	}

	static makeplot1($cont,dag,me) {
		let {par,kid} = View.me2parkid(dag,me)
		$cont.append(`<span id='${me}' style='border:thin solid black;width:100px;height:50px;color:#999'>${me}</span>`)
		return () => {
			let frame = Frame.fromString(get_input(par))
			$('#'+me).empty()
			$('#'+me).removeAttr('style')
			$('#'+me).append(`<table><tr><td id='${me}2' width='400px'></td></tr></table>`)
			Plot.fromFrame(frame).plot1(me+'2')
		}
	}

	static makeplot2($cont,dag,me) {
		let {par,kid} = View.me2parkid(dag,me)
		$cont.append(`<div id='${me}' style='border:thin solid black;width:100px;height:50px;color:#999'>${me}</div>`)
		return () => {
			let frame = Frame.fromString(get_input(par))
			let frame1 = frame.copy().removecol(1)
			let frame2 = frame.copy().removecol(0)
			if (frame.numcols() >= 3) {
				$('#'+me).empty()
				$('#'+me).removeAttr('style')
				$('#'+me).append(`<table><tr><td id='${me}1' width='500px'></td><td id='${me}2' width='500px'></td><td id='${me}3' width='500px'></td></tr></table>`)
				Plot.fromFrame(frame1).plot1(me+1)
				Plot.fromFrame(frame ).plot2(me+2)
				Plot.fromFrame(frame2).plot1(me+3)
			}			
		}
	}

	static makeplot23($cont,dag,me) {
		let {par,kid} = View.me2parkid(dag,me)
		$cont.append(`<div id='${me}' style='border:thin solid black;width:100px;height:50px;color:#999'>${me}</div>`)
		return () => {
			let frame = Frame.fromString(get_input(par))
			let frame1 = frame.copy().removecol(1)
			let frame2 = frame.copy().removecol(0)
			if (frame.numcols() >= 3) {
				$('#'+me).empty()
				$('#'+me).removeAttr('style')
				$('#'+me).append(`<table><tr><td id='${me}1' width='500px'></td><td id='${me}2' width='500px'></td><td id='${me}3' width='500px'></td></tr></table>`)
				Plot.fromFrame(frame1).plot1 (me+1)
				Plot.fromFrame(frame ).plot23(me+2)
				Plot.fromFrame(frame2).plot1 (me+3)
			}			
		}
	}

	static makeplot2layer($cont,dag,me) {
		let {par,kid} = View.me2parkid(dag,me)
		$cont.append(`<div id='${me}' style='border:thin solid black;width:100px;height:50px;color:#999'>${me}</div>`)
		return () => {
			$('#'+me).empty()
			$('#'+me).removeAttr('style')
			$('#'+me).append(`<table><tr><td id='${me}2' width='500px'></td></tr></table>`)
			Plot.fromString(get_input(par)).plot2layer(me+2)
		}
	}

	static makecause($cont,dag,me) {
		let {par,kid} = View.me2parkid(dag,me)
		$cont.append(`<textarea id='${me}' cols='50' rows='7' placeholder='${me}'></textarea>`)
		return ()=>{
			let csv = get_input(par)
			let model = new Model(Frame.fromString(csv))
			let middle = model.get_control_name()
			let ends = model.get_noncontrol_names()
			let graph = ends[0] + '-' + middle + '-' + ends[1]
			set_textarea(me,graph)
		}
	}

	static makef($cont,dag,me,f) {
		let {par,kid} = View.me2parkid(dag,me)
		$cont.append(`<textarea id='${me}' placeholder='${me}' cols='30'></textarea>`)
		return ()=>$('#'+me).val(f(...par.map(p=>$('#'+p).val())))
	}

}
