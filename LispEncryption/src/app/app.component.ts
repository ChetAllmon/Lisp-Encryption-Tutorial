import { Component } from '@angular/core';

@Component({
  selector: 'app-root',
  templateUrl: './app.component.html',
  styleUrls: ['./app.component.css']
})
export class AppComponent {
  title = 'LispEncryption';

  rotationBoxes = 
  [{letter: "A"}, {letter:"B"},{letter: "C"},{letter:"D"},
   {letter: "E"}, {letter:"F"},{letter: "G"}, {letter:"H"},
   {letter: "J"}, {letter:"K"},{letter: "L"}, {letter:"M"},
   {letter: "N"}, {letter:"O"},{letter: "P"}, {letter:"Q"},
   {letter: "R"}, {letter:"S"},{letter: "T"}, {letter:"U"},
   {letter: "V"}, {letter:"W"},{letter: "X"}, {letter:"Y"},
   {letter: "Z"}
  ]
 
  // keyValue = document.getElementById("key");
  keyValue  = "0";
  getTextBoxVal(key){
    this.keyValue = key.value
  }

}



  
