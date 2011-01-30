<html>
<script src="https://ajax.googleapis.com/ajax/libs/jquery/1.4.4/jquery.min.js"></script>
<textarea id='text-area'>Enter messages here...</textarea><br>
<button id='submit'>Submit</button>
<script>
$("#submit").bind("click", function(){
  $.getJSON("/message",{data: $("#text-area").val()});
});
</script>
</html>
