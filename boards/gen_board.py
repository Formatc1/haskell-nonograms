from selenium import webdriver
import sys
import operator

if len(sys.argv) < 2:
    sys.stderr.write('Usage: python %s url_to_nonogram' % sys.argv[0])
    sys.exit(1)

driver = webdriver.PhantomJS('phantomjs')
driver.get(sys.argv[1])
# driver.get('http://www.nonograms.org/nonograms/i/4883')
size = driver.find_element_by_css_selector(
    '.content > table > tbody > tr > td').text.split(' ')[1]
result = size.replace('x', ' ')

tds = driver.find_elements_by_css_selector("table#nonogram_table td.num")
vertical = [{'col': int(x.get_attribute('id')[3:].split('_')[0]),
             'row': int(x.get_attribute('id')[3:].split('_')[1]),
             'text': x.text}
            for x in tds if x.get_attribute('id')[0:3] == 'nmv']
vertical = sorted(vertical, key=operator.itemgetter('col', 'row'))
horizontal = [{'col': int(x.get_attribute('id')[3:].split('_')[0]),
               'row': int(x.get_attribute('id')[3:].split('_')[1]),
               'text': x.text}
              for x in tds if x.get_attribute('id')[0:3] == 'nmh']
horizontal = sorted(horizontal, key=operator.itemgetter('row', 'col'))

col = -1
row = -1
for x in vertical:
    if x['col'] == col:
        result += (' ' + x['text'])
    else:
        result += ('\n' + x['text'])
        col = x['col']

for x in horizontal:
    if x['row'] == row:
        result += (' ' + x['text'])
    else:
        result += ('\n' + x['text'])
        row = x['row']

driver.quit()

print 'Saved as: ' + size + '.txt'

with open(size + '.txt', 'w+') as board:
    board.write(result)
