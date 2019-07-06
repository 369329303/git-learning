<html>
    <head>
	<title>Download file from Internet</title>
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
	    echo exec("pwd");
	    $cmd = "cd /var/www/html/res/ && curl -s -L -O " . $url;
	    exec($cmd);
	    echo " <b>" . $output . "</b> " . $return_var;
	    echo "<br /><br />";
	    echo "The link [ <font color='green'>" . $url . "</font> ] has been downloaded!";
	}
	?>
    </body>
</html>
