FROM nginx:1.26-alpine

WORKDIR /etc/nginx

COPY nginx/nginx.conf /etc/nginx/conf.d/default.conf

EXPOSE 80

CMD ["nginx", "-g", "daemon off;"]
