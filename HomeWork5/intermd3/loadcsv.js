// This is a javascript file to load the csv .

// This is dynamically included in i_index.html


var dataset; //Declare global variable, initially empty (undefined)


d3.csv("presidents.csv",  function(error, data) {
	
	if (error) { 					//If error is not null, something went wrong.
		console.log(error); 		//Log the error.
	} else { 						//If no error, the file loaded correctly. 
		dataset = data;
		console.log(data);
		console.table(dataset, ["Name", "Height", "Weight"]);
		

		// Build Table of Presidents data      

        var presitable = document.createElement("table");
        presitable.width = '25%' ;
        presitable.cellSpacing = '0' ;

     
        presitable.frame='box';

		// Header For Table        

		var row = presitable.insertRow(-1);




		//For Headers
		
		var headerName = row.insertCell(0);
        var headerHeight = row.insertCell(1);
        var headerWeight = row.insertCell(2);

        headerName.innerHTML = "Name";
        headerHeight.innerHTML = "Height";
        headerWeight.innerHTML = "Weight";
		headerName.style.fontWeight="bold";
		headerHeight.style.fontWeight="bold";
		headerWeight.style.fontWeight="bold";
		headerName.style.color="black";
		headerHeight.style.color="black";
		headerWeight.style.color="black";
		headerName.style.background="aqua";
		headerHeight.style.background="aqua";
		headerWeight.style.background="aqua";

		// For Loading the data in table cells

        for(var i=0; i<dataset.length; i=i+1) {
            
            
                var row = presitable.insertRow(-1);
                var cell1 = row.insertCell(0);
                var cell2 = row.insertCell(1);
                var cell3 = row.insertCell(2);
                
                
                cell1.innerHTML = dataset[i].Name;
                cell2.innerHTML = dataset[i].Height;
                cell3.innerHTML = dataset[i].Weight;
                

        }
    document.getElementById("addtable").appendChild(presitable);

        var cap = document.getElementById("addTable").createCaption();
        cap.innerHTML="<b>President Physique Table</b><br><br>"    	


	} // End of else
}
);

