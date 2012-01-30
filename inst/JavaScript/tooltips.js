
    var SVGDocument = null;
    var SVGRoot = null;
    var SVGViewBox = null;
    var svgns = 'http://www.w3.org/2000/svg';
    var xlinkns = 'http://www.w3.org/1999/xlink';
    var toolTip = null;
    var TrueCoords = null;
    var tipBox = null;
    var tipText = null;
    var tipTitle = null;
    var tipDesc = null;

    var lastElement = null;
    var titleText = '';
    var titleDesc = '';


    function Init(evt)
    {
       SVGDocument = evt.target.ownerDocument;
       SVGRoot = SVGDocument.documentElement;
       TrueCoords = SVGRoot.createSVGPoint();

       toolTip = SVGDocument.getElementById('ToolTip');
	var newline = SVGDocument.createTextNode("dd");	
	  toolTip.appendChild(newline);

       tipBox = SVGDocument.getElementById('tipbox');
       tipText = SVGDocument.getElementById('tipText');
       tipText.setAttributeNS(null, 'font-family', 'Arial, sans-serif' );
       tipTitle = SVGDocument.getElementById('tipTitle');
       tipDesc = SVGDocument.getElementById('tipDesc');
       //window.status = (TrueCoords);

       //create event for object
       SVGRoot.addEventListener('mouseover', ShowTooltip, false);
       SVGRoot.addEventListener('mouseout', HideTooltip, false);
       SVGRoot.addEventListener('mousemove', moveTooltip, false);

    };


    function GetTrueCoords(evt)
    {
       // find the current zoom level and pan setting, and adjust the reported
       //    mouse position accordingly
       var newScale = SVGRoot.currentScale;
       var translation = SVGRoot.currentTranslate;
       TrueCoords.x = (evt.clientX - translation.x)/newScale;
       TrueCoords.y = (evt.clientY - translation.y)/newScale;
    };


    function HideTooltip( evt )
    {

       toolTip.setAttributeNS(null, 'visibility', 'hidden');
	while(tipText.children.length>2)
	{
	       tipText.removeChild(tipText.lastChild);
	}
	
    };

  function moveTooltip(evt)
 {
	GetTrueCoords( evt );
       var tipScale = 1/SVGRoot.currentScale;
          var xPos = TrueCoords.x + (10 * tipScale);
          var yPos = TrueCoords.y + (10 * tipScale);

          // update position
        toolTip.setAttributeNS(null, 'transform', 'translate(' + xPos + ',' + yPos + ')');
//        toolTip.setAttributeNS(null, 'visibility', 'visible');

 }

    function ShowTooltip( evt )
    {
       GetTrueCoords( evt );

       var tipScale = 1/SVGRoot.currentScale;
       var textWidth = 0;
       var tspanWidth = 0;
       var boxHeight = 20;

       tipBox.setAttributeNS(null, 'transform', 'scale(' + tipScale + ',' + tipScale + ')' );
       tipText.setAttributeNS(null, 'transform', 'scale(' + tipScale + ',' + tipScale + ')' );

	//clear tooltips
     // tipTitle.contentText='';
      //tipDesc.contentText='';
      //tipTitle.setAttributeNS(null, 'display', 'none' );
      //tipDesc.setAttributeNS(null, 'display', 'none' );
       var titleValue = '';
       var descValue = '';
       var targetElement = evt.target;
       if ( lastElement != targetElement )
       {
          var targetTitle = targetElement.getElementsByTagNameNS(svgns, 'title').item(0);
          if ( targetTitle )
          {
             // if there is a 'title' element, use its contents for the tooltip title
             titleValue = targetTitle.firstChild.nodeValue;
          }

          var targetDesc = targetElement.getElementsByTagNameNS(svgns, 'desc').item(0);
          if ( targetDesc )
          {
             // if there is a 'desc' element, use its contents for the tooltip desc
             descValue = targetDesc.firstChild.nodeValue;

             //if ( '' == titleValue )
             //{
                // if there is no 'title' element, use the contents of the 'desc' element for the tooltip title instead
                //titleValue = descValue;
                //descValue = '';
             //}
          }

          // if there is still no 'title' element, use the contents of the 'id' attribute for the tooltip title
          //if ( '' == titleValue )
          //{
            // titleValue = targetElement.getAttributeNS(null, 'id');
          //}

          // selectively assign the tooltip title and desc the proper values,
          //   and hide those which don't have text values
          //
          var titleDisplay = 'none';
          if ( '' != titleValue )
          {
             tipTitle.firstChild.nodeValue = titleValue;
             titleDisplay = 'inline';
          }
          tipTitle.setAttributeNS(null, 'display', titleDisplay );


          var descDisplay = 'none';
          if ( '' != descValue )
          {
             tipDesc.firstChild.nodeValue = descValue;
             descDisplay = 'inline';

		//split seqs into mulitple lines when applicable
		var seqs=descValue.split("|");
		tipDesc.textContent=seqs[0];
		for(i=1;i<seqs.length;i++)
		{
	   	 str=seqs[i];
		 if(str.length>0)		
		 {
		  newline=tipDesc.cloneNode(true);
		  newline.textContent=str;
		  var id="desc"+i;
		  newline.setAttributeNS(null, 'id', id);
		  tipText.appendChild(newline);
		  newline.setAttributeNS(null, 'display', descDisplay );
		 }
		}

          }
          tipDesc.setAttributeNS(null, 'display', descDisplay );
       }


       // if there are tooltip contents to be displayed, adjust the size and position of the box
       if ( '' != titleValue||'' != descValue )
       {

          var xPos = TrueCoords.x + (10 * tipScale);
          var yPos = TrueCoords.y + (10 * tipScale);

	
          //return rectangle around text as SVGRect object
          var outline = tipText.getBBox();
	  realHeight=tipText.children.length*Number(outline.height)/2;
          tipBox.setAttributeNS(null, 'width', Number(outline.width) + 10);
          tipBox.setAttributeNS(null, 'height', Number(realHeight) + 10);
	  tipBox.setAttributeNS(null, 'visibility', 'hidden');

          // update position
        toolTip.setAttributeNS(null, 'transform', 'translate(' + xPos + ',' + yPos + ')');
        toolTip.setAttributeNS(null, 'visibility', 'visible');
       }
    };


