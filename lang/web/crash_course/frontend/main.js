const ul = document.querySelector(".items");

ul.children[1].innerText = 'Brad'
ul.lastElementChild.innerHTML = '<h1>H1</h1>'

const mf = document.querySelector('#my-form')
mf.style.backgroundColor = '0x0f0'

const btn = document.querySelector('#btn')
btn.addEventListener('click', (e) => {
    e.preventDefault();
    console.log(e.target.id)
})