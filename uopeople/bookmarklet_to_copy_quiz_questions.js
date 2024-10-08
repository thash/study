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

        let optionsText = '';
        q.querySelectorAll('.answer > div').forEach(o => {
            let n = o.querySelector('.answernumber')?.innerText.trim();
            let t = o.querySelector('.flex-fill')?.innerText.trim();
            if (n && t) optionsText += `- ${n} ${t}\n`;
        });

        if (/^- a\. (True|False)/i.test(optionsText.trim())) {
            output += `True/False: ${qt}\n\n\n`;
        } else {
            output += `${qt}\n${optionsText}\n\n`;
        }
    });

    navigator.clipboard.writeText(output).then(() => {
        alert('Questions and options have been copied to the clipboard.');
    }).catch(err => {
        alert('Failed to copy text to clipboard:', err);
    });
})();
