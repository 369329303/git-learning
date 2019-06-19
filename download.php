<html>
    <head>
	<style>
	 body {
	     text-align: center;
	 }
	 form {
	     margin: 50px;
	     display: inline-block;
	 }
	</style>
    </head>
    <body>
	<form action="download.php" method="POST">
	    <input name="url">
	    <input type="submit">
	</form>
	<?php
	$url = $_POST["url"];
	if (isset($url)) {
	    $cmd = "wget -P /var/www/html/res/ " . $url;
	    /* echo $cmd; */
	    /* exec($cmd); */
	    echo "<br /><br />";
	    echo "The link [ <font color='red'>" . $url . "</font> ] has been downloaded!";
	}
	?>
    </body>
</html>
