// Master Javascript for weather.gov
// Mostly legacy stuff to support the 3 day city forecast pop up window 
// 


function init() {
if (top.frames.length!=0)
top.location=self.document.location;
}

function doArray() {
    var arylnth = doArray.arguments.length;
    for ( i = 0 ; i < arylnth ; i++ ) { this[i]=doArray.arguments[i] }
}


function statepage() {
    var choose = eval(document.dropdown1.state.selectedIndex);
    if( (choose >= 1) && (choose < 55) ) {
        var section=new doArray(
                '',
                'javascript:void window.open("Alabama.html","AL","width=450,height=350,scrollbars,toolbar")',
                'javascript:void window.open("Alaska.html","AK","width=450,height=350,scrollbars,toolbar")',
                'javascript:void window.open("Arizona.html","AZ","width=450,height=350,scrollbars,toolbar")',
                'javascript:void window.open("Arkansas.html","AR","width=450,height=350,scrollbars,toolbar")',
                'javascript:void window.open("Calif.html","CA","width=450,height=350,scrollbars,toolbar")',
                'javascript:void window.open("Colorado.html","CO","width=450,height=350,scrollbars,toolbar")',
                'javascript:void window.open("Conn.html","CT","width=450,height=350,scrollbars,toolbar")',
                'javascript:void window.open("Delaware.html","DE","width=450,height=350,scrollbars,toolbar")',
                'javascript:void window.open("DC.html","DC","width=530,height=350,scrollbars,toolbar")',
                'javascript:void window.open("Florida.html","FL","width=450,height=350,scrollbars,toolbar")',
                'javascript:void window.open("Georgia.html","GA","width=450,height=350,scrollbars,toolbar")',
                'javascript:void window.open("Hawaii.html","HI","width=450,height=350,scrollbars,toolbar")',
                'javascript:void window.open("Idaho.html","ID","width=450,height=350,scrollbars,toolbar")',
                'javascript:void window.open("Illinois.html","IL","width=450,height=350,scrollbars,toolbar")',
                'javascript:void window.open("Indiana.html","IN","width=450,height=350,scrollbars,toolbar")',
                'javascript:void window.open("Iowa.html","IA","width=450,height=350,scrollbars,toolbar")',
                'javascript:void window.open("Kansas.html","KS","width=450,height=350,scrollbars,toolbar")',
                'javascript:void window.open("Kentucky.html","KY","width=450,height=350,scrollbars,toolbar")',
                'javascript:void window.open("Louisiana.html","LA","width=450,height=350,scrollbars,toolbar")',
                'javascript:void window.open("Maine.html","ME","width=450,height=350,scrollbars,toolbar")',
                'javascript:void window.open("Maryland.html","MD","width=450,height=350,scrollbars,toolbar")',
                'javascript:void window.open("Mass.html","MA","width=450,height=350,scrollbars,toolbar")',
                'javascript:void window.open("Michigan.html","MI","width=450,height=350,scrollbars,toolbar")',
                'javascript:void window.open("Minn.html","MN","width=450,height=350,scrollbars,toolbar")',
                'javascript:void window.open("Miss.html","MS","width=450,height=350,scrollbars,toolbar")',
                'javascript:void window.open("Missouri.html","MO","width=450,height=350,scrollbars,toolbar")',
                'javascript:void window.open("Montana.html","MT","width=450,height=350,scrollbars,toolbar")',
                'javascript:void window.open("Nebraska.html","NE","width=450,height=350,scrollbars,toolbar")',
                'javascript:void window.open("Nevada.html","NV","width=450,height=350,scrollbars,toolbar")',
                'javascript:void window.open("NewHam.html","NH","width=450,height=350,scrollbars,toolbar")',
                'javascript:void window.open("Jersey.html","NJ","width=450,height=350,scrollbars,toolbar")',
                'javascript:void window.open("Newmex.html","NM","width=450,height=350,scrollbars,toolbar")',
                'javascript:void window.open("NewYork.html","NY","width=450,height=350,scrollbars,toolbar")',
                'javascript:void window.open("NorthCaro.html","NC","width=450,height=350,scrollbars,toolbar")',
                'javascript:void window.open("NorthDak.html","ND","width=450,height=350,scrollbars,toolbar")',
                'javascript:void window.open("Ohio.html","OH","width=450,height=350,scrollbars,toolbar")',
                'javascript:void window.open("Oklahoma.html","OK","width=450,height=350,scrollbars,toolbar")',
                'javascript:void window.open("Oregon.html","OR","width=450,height=350,scrollbars,toolbar")',
                'javascript:void window.open("Penn.html","PA","width=450,height=350,scrollbars,toolbar")',
                'javascript:void window.open("Puerto.html","PR","width=450,height=350,scrollbars,toolbar")',
                'javascript:void window.open("RhodeIsl.html","RI","width=450,height=350,scrollbars,toolbar")',
                'javascript:void window.open("SouthCaro.html","SC","width=450,height=350,scrollbars,toolbar")',
                'javascript:void window.open("SouthDak.html","SD","width=450,height=350,scrollbars,toolbar")',
                'javascript:void window.open("Tenn.html","TN","width=450,height=350,scrollbars,toolbar")',
                'javascript:void window.open("Texas.html","TX","width=450,height=350,scrollbars,toolbar")',
                'javascript:void window.open("Utah.html","UT","width=450,height=350,scrollbars,toolbar")',
                'javascript:void window.open("Vermont.html","VT","width=450,height=350,scrollbars,toolbar")',
                'javascript:void window.open("Virginia.html","VA","width=450,height=350,scrollbars,toolbar")',
                'javascript:void window.open("Virgin.html","VA","width=450,height=350,scrollbars,toolbar")',
                'javascript:void window.open("Washington.html","WA","width=450,height=350,scrollbars,toolbar")',
                'javascript:void window.open("WestVirg.html","WV","width=450,height=350,scrollbars,toolbar")',
                'javascript:void window.open("Wisconsin.html","WI","width=450,height=350,scrollbars,toolbar")',
                'javascript:void window.open("Wyoming.html","WY","width=450,height=350,scrollbars,toolbar")');
        
        var targetloc=section[document.dropdown1.state.selectedIndex];
        document.dropdown1.state.options[document.dropdown1.state.selectedIndex].selected = false;
        document.dropdown1.state.options[0].selected = true;
        location=targetloc;
    }
}

