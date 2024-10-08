javascript:(function(){
    let output = '';
    document.querySelectorAll('div[id^="question-"]').forEach(q => {
        let qt = q.querySelector('.qtext')?.innerText.trim()
            .replace(/_+/g, (_, i, s) => {
                let b = s[i - 1], a = s[i + _.length];
                let pre = b && !/\s/.test(b) ? ' ' : '';
                let post = a && !/\s/.test(a) ? ' ' : '';
                return `${pre}((blank))${post}`;
            })
            .replace(/\s+/g, ' ').trim();
        output += `${qt}\n`;
        q.querySelectorAll('.answer > div').forEach(o => {
            let n = o.querySelector('.answernumber')?.innerText.trim();
            let t = o.querySelector('.flex-fill')?.innerText.trim();
            if (n && t) output += `- ${n} ${t}\n`;
        });
        output += '\n\n';
    });
    window.prompt('Copy the extracted questions below:', output);
})();
