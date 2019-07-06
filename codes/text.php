<?php
$text = $_POST["text"];
if (isset($text))
   file_put_contents("text.dat", trim($text));
?>

<html>
    <head>
	<style>
	 body {
	     text-align: center;
	 }
	 form {
	     display: inline-block;
	 }
	</style>
    </head>
    <body>
	<form action="text.php" method="post" class="center">
	    <textarea name="text" cols="50" rows="10">
		<?php
		$fh = fopen('text.dat','r');
		while ($line = fgets($fh))
		    echo($line);
		fclose($fh);
		?>
	    </textarea><br /><br />
	    <input type="submit">
	</form>
    </body>
</html>
