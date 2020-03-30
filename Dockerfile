FROM nginx
ADD dist /usr/share/nginx/html/
ENV PORT 80
COPY nginx.conf /etc/nginx/nginx.conf.tmpl
CMD envsubst < /etc/nginx/nginx.conf.tmpl > /etc/nginx/nginx.conf && exec nginx -g 'daemon off;'
